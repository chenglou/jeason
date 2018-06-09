open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

/* helpers */
/* TODO: turn foo_bar into foo_bar_ */
let correctIdentifier = ident => {
  let rec stripLeadingUnderscores = s =>
    if (String.length(s) == 0) {
      s;
    } else if (s.[0] == '_') {
      stripLeadingUnderscores(String.sub(s, 1, String.length(s) - 1));
    } else {
      s;
    };
  /* ocaml/reason identifiers need to be lower-cased (uppercase reserved for variants constructors, modules, etc.) */
  if (ident == "") {
    ident;
  } else {
    /* foo => foo
       Foo => foo
       _foo => foo
       _foo_bar => foo_bar_ */
    let correctedName = stripLeadingUnderscores(ident);
    let correctedName =
      String.contains(correctedName, '_') ?
        correctedName ++ "_" : correctedName;
    let correctedName = String.uncapitalize(correctedName);
    /* correct other cases where the js name is a reserved ocaml/reason keyword */
    switch (correctedName) {
    | "object" => "object_"
    | "type" => "type_"
    | n => n
    };
  };
};

let objectContainsKeyName = (~key, ~properties) =>
  Parser_flow.Ast.Expression.(
    properties
    |> List.exists(property =>
         switch (property) {
         | Object.Property((
             _,
             {Object.Property.key: Object.Property.Identifier((_, name))},
           ))
             when name == key =>
           true
         | _ => false
         }
       )
  );

let astHelperStrLidStr = (~correct=true, a) => {
  loc: default_loc.contents,
  txt: correct ? correctIdentifier(a) : a,
};

let astHelperStrLidIdent = (~correct=true, a) =>
  switch (a) {
  | [] => raise(Invalid_argument("identifier is empty."))
  | _ =>
    let inner =
      Lident(correct ? correctIdentifier(List.hd(a)) : List.hd(a));
    let res =
      List.tl(a)
      |> List.fold_left(
           (acc, curr) =>
             Ldot(acc, correct ? correctIdentifier(curr) : curr),
           inner,
         );
    {loc: default_loc.contents, txt: res};
  };

let expUnit =
  Exp.construct(astHelperStrLidIdent(~correct=false, ["()"]), None);

/* using this as a convenient placeholder output, for checking whether I've matched the js ast correctly */
let placeholder = s => Exp.ident(astHelperStrLidIdent([s]));

let expMarker = placeholder("marker");

let parseTreeValueBinding = (~pat, ~expr) => {
  pvb_pat: pat,
  pvb_expr: expr,
  pvb_attributes: [],
  pvb_loc: default_loc.contents,
};

let keepSome = lst =>
  lst
  |> List.filter(a =>
       switch (a) {
       | None => false
       | _ => true
       }
     )
  |> List.map(a =>
       switch (a) {
       | None => assert(false)
       | Some(a) => a
       }
     );

let listToListAst = lst => {
  let nullList =
    Exp.construct(astHelperStrLidIdent(~correct=false, ["[]"]), None);
  /* we transform js array to BS array, except in the case of jsx which takes a reason list instead */
  switch (lst) {
  | [] => nullList
  | oneItemOrMore =>
    List.rev(oneItemOrMore)
    |> List.fold_left(
         (accumExp, expr) =>
           Exp.construct(
             astHelperStrLidIdent(~correct=false, ["::"]),
             Some(Exp.tuple([expr, accumExp])),
           ),
         nullList,
       )
  };
};

type insideReactClass =
  | Nope
  | InsidePropTypes
  | Yes(string);

type context = {
  terminalExpr: option(Parsetree.expression),
  insideReactClass,
  mutable reactClassSpecRandomProps: list(list(string)),
};

let rec convertPropTypeType =
        (~isTopLevel, ~isForFunctionLabel, ~wrappedByRequired, {pexp_desc}) => {
  let wrapIfNotRequired = (~attrs=[], ident) => {
    let partialResult = {
      ptyp_loc: default_loc.contents,
      ptyp_attributes: attrs,
      ptyp_desc: ident,
    };
    if (wrappedByRequired) {
      partialResult;
    } else if (isTopLevel && isForFunctionLabel) {
      {
        ptyp_loc: default_loc.contents,
        ptyp_attributes: [],
        ptyp_desc:
          /* weird ast rule for 4.02: if you want to create an optional labelled argument type, you need to
             prepend the name of the label with a question mark (see propTypesToActualTypes) and have the
             special *predef* node */
          Ptyp_constr(
            astHelperStrLidIdent(~correct=false, ["*predef*", "option"]),
            [
              {
                ptyp_loc: default_loc.contents,
                ptyp_attributes: [],
                ptyp_desc:
                  Ptyp_constr(
                    {
                      loc: default_loc.contents,
                      txt: Ldot(Lident("Js"), "null_undefined"),
                    },
                    [partialResult],
                  ),
              },
            ],
          ),
      };
    } else {
      {
        ptyp_loc: default_loc.contents,
        ptyp_attributes: [],
        ptyp_desc:
          Ptyp_constr(
            {
              loc: default_loc.contents,
              txt: Ldot(Lident("Js"), "null_undefined"),
            },
            [partialResult],
          ),
      };
    };
  };
  let unsupported = str =>
    wrapIfNotRequired(Ptyp_constr(astHelperStrLidIdent([str]), []));
  switch (pexp_desc) {
  | Pexp_ident({
      txt:
        Ldot(
          Ldot(Lident("ReactRe"), "PropTypes"),
          propNameThatsNotRequired,
        ),
    }) =>
    switch (propNameThatsNotRequired) {
    | "string" =>
      wrapIfNotRequired(Ptyp_constr(astHelperStrLidIdent(["string"]), []))
    | "bool" =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(~correct=false, ["Js", "boolean"]),
          [],
        ),
      )
    /* TODO: smart detection */
    | "number" =>
      wrapIfNotRequired(Ptyp_constr(astHelperStrLidIdent(["int"]), []))
    | "node" =>
      wrapIfNotRequired(
        Ptyp_constr(astHelperStrLidIdent(["nodeTypeNotSupported"]), []),
      )
    | "object_" =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(~correct=false, ["Js", "t"]),
          [
            {
              ptyp_loc: default_loc.contents,
              ptyp_attributes: [],
              ptyp_desc:
                Ptyp_constr(
                  astHelperStrLidIdent(~correct=false, ["Obj", "t"]),
                  [],
                ),
            },
          ],
        ),
      )
    | "symbol" =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(["symbolTypeNotSupportedHere"]),
          [],
        ),
      )
    | "any" =>
      wrapIfNotRequired(
        Ptyp_constr(astHelperStrLidIdent(~correct=false, ["Obj", "t"]), []),
      )
    | "element" =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(~correct=false, ["ReactRe", "reactElement"]),
          [],
        ),
      )
    | "func" =>
      wrapIfNotRequired(
        ~attrs=[(astHelperStrLidStr("bs.meth"), PStr([]))],
        Ptyp_arrow(
          "",
          {
            ptyp_loc: default_loc.contents,
            ptyp_attributes: [],
            ptyp_desc: Ptyp_constr(astHelperStrLidIdent(["unit"]), []),
          },
          {
            ptyp_loc: default_loc.contents,
            ptyp_attributes: [],
            ptyp_desc: Ptyp_constr(astHelperStrLidIdent(["unit"]), []),
          },
        ),
      )
    | _ =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(["cannotUnderstandPropTypeHere"]),
          [],
        ),
      )
    }
  | Pexp_apply(
      {
        pexp_desc:
          Pexp_ident({
            txt: Ldot(Ldot(Lident("ReactRe"), "PropTypes"), propName),
          }),
      },
      [(_, expr)],
    ) =>
    switch (propName) {
    | "isRequired" =>
      convertPropTypeType(
        ~isTopLevel=false,
        ~isForFunctionLabel,
        ~wrappedByRequired=true,
        expr,
      )
    | "oneOfType" => unsupported("oneOfTypeUnSupported")
    | "oneOf" => unsupported("oneOfUnSupported")
    | "objectOf" => unsupported("objectOfUnSupported")
    | "instanceOf" => unsupported("instanceOfUnSupported")
    | "arrayOf" =>
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(["array"]),
          [
            convertPropTypeType(
              ~isTopLevel=false,
              ~isForFunctionLabel,
              ~wrappedByRequired=false,
              expr,
            ),
          ],
        ),
      )
    | "shape" =>
      let convertedFields =
        switch (expr.pexp_desc) {
        | Pexp_extension((
            _,
            PStr([
              {
                pstr_desc: Pstr_eval({pexp_desc: Pexp_record(fields, _)}, _),
              },
            ]),
          )) =>
          fields
          |> List.map((({txt}, expr)) =>
               switch (txt) {
               | Lident(propName) => (
                   propName,
                   [],
                   convertPropTypeType(
                     ~isTopLevel=false,
                     ~isForFunctionLabel,
                     ~wrappedByRequired=false,
                     expr,
                   ),
                 )
               | _ => (
                   "cannotGenerateType",
                   [],
                   {
                     ptyp_loc: default_loc.contents,
                     ptyp_attributes: [],
                     ptyp_desc:
                       Ptyp_constr(
                         astHelperStrLidIdent([
                           "forComplexPropTypesObjectKey",
                         ]),
                         [],
                       ),
                   },
                 )
               }
             )
        | _ => []
        };
      wrapIfNotRequired(
        Ptyp_constr(
          astHelperStrLidIdent(~correct=false, ["Js", "t"]),
          [
            {
              ptyp_loc: default_loc.contents,
              ptyp_attributes: [],
              ptyp_desc: Ptyp_object(convertedFields, Closed),
            },
          ],
        ),
      );
    | _ => unsupported("unrecognizedPropType")
    }
  | _ => unsupported("unrecognizedPropType")
  };
};

let propTypesToActualTypes = fields => {
  /* reference: go from

     let propTypes = {
       "inner": PropTypes.isRequired PropTypes.number,
       "something": PropTypes.string,
       "children": PropTypes.element
     };

     to:

     type props = {
       inner : int,
       something : option string,
       children : option ReactRe.reactElement
     };

     let jsPropsToReasonProps (jsProps: jsPropsType) :props => {
       inner : jsProps##inner,
       something : Js.Null_undefined.to_opt jsProps##something,
       children : Js.Null_undefined.to_opt jsProps##children
     };

     not all implemented yet. */
  let convertedFieldsForProps =
    fields
    |> List.map((({txt}, expr)) =>
         switch (txt) {
         | Lident(propName) => (
             propName,
             [],
             convertPropTypeType(
               ~isTopLevel=true,
               ~isForFunctionLabel=false,
               ~wrappedByRequired=false,
               expr,
             ),
           )
         | _ => (
             "cannotGenerateType",
             [],
             {
               ptyp_loc: default_loc.contents,
               ptyp_attributes: [],
               ptyp_desc:
                 Ptyp_constr(
                   {
                     loc: default_loc.contents,
                     txt: Lident("forComplexPropTypesObjectKey"),
                   },
                   [],
                 ),
             },
           )
         }
       );
  let propsObjType =
    Str.type_([
      Type.mk(
        ~kind=Ptype_abstract,
        ~priv=Public,
        ~manifest={
          ptyp_loc: default_loc.contents,
          ptyp_attributes: [],
          ptyp_desc:
            Ptyp_constr(
              {loc: default_loc.contents, txt: Ldot(Lident("Js"), "t")},
              [
                {
                  ptyp_loc: default_loc.contents,
                  ptyp_attributes: [],
                  ptyp_desc: Ptyp_object(convertedFieldsForProps, Closed),
                },
              ],
            ),
        },
        astHelperStrLidStr("props"),
      ),
    ]);
  let inner = {
    ptyp_loc: default_loc.contents,
    ptyp_attributes: [],
    ptyp_desc:
      Ptyp_arrow(
        "",
        {
          ptyp_loc: default_loc.contents,
          ptyp_attributes: [],
          ptyp_desc: Ptyp_constr(astHelperStrLidIdent(["unit"]), []),
        },
        {
          ptyp_loc: default_loc.contents,
          ptyp_attributes: [],
          ptyp_desc: Ptyp_var("reactJsProps"),
        },
      ),
  };
  let externalTypeInner =
    fields
    |> List.rev
    |> List.fold_left(
         (acc, ({txt}, {pexp_desc} as expr)) => {
           let topLevelIsRequired =
             switch (pexp_desc) {
             | Pexp_apply(
                 {
                   pexp_desc:
                     Pexp_ident({
                       txt:
                         Ldot(
                           Ldot(Lident("ReactRe"), "PropTypes"),
                           "isRequired",
                         ),
                     }),
                 },
                 [(_, expr)],
               ) =>
               true
             | _ => false
             };
           let label =
             switch (txt) {
             | Lident(propName) =>
               topLevelIsRequired ? propName : "?" ++ propName
             | _ => "complexPropTypesObjectKey"
             };
           {
             ptyp_loc: default_loc.contents,
             ptyp_attributes: [],
             ptyp_desc:
               Ptyp_arrow(
                 label,
                 convertPropTypeType(
                   ~isTopLevel=true,
                   ~isForFunctionLabel=true,
                   ~wrappedByRequired=false,
                   expr,
                 ),
                 acc,
               ),
           };
         },
         inner,
       );
  let externalType =
    Str.primitive({
      pval_name: {
        loc: default_loc.contents,
        txt: "props",
      },
      pval_prim: [""],
      pval_loc: default_loc.contents,
      pval_type: externalTypeInner,
      pval_attributes: [
        ({loc: default_loc.contents, txt: "bs.obj"}, PStr([])),
      ],
    });
  (propsObjType, externalType);
};

let attemptToGenerateStateRecord = initialStateDeclaration => {
  let rec innerMostExpr = pexp_desc =>
    switch (pexp_desc) {
    | Pexp_extension((
        {txt: "bs.obj"},
        PStr([
          {pstr_desc: Pstr_eval({pexp_desc: Pexp_record(lst, _)}, _)},
        ]),
      )) =>
      Some(lst)
    | Pexp_let(_, _, {pexp_desc})
    | Pexp_sequence(_, {pexp_desc})
    | Pexp_constraint({pexp_desc}, _) => innerMostExpr(pexp_desc)
    | _ => None
    };
  let bailType =
    Str.type_([
      Type.mk(
        ~kind=Ptype_abstract,
        ~priv=Public,
        ~manifest={
          ptyp_loc: default_loc.contents,
          ptyp_attributes: [],
          ptyp_desc:
            Ptyp_constr(
              {loc: default_loc.contents, txt: Ldot(Lident("Js"), "t")},
              [
                {
                  ptyp_loc: default_loc.contents,
                  ptyp_attributes: [],
                  ptyp_desc:
                    Ptyp_object(
                      [
                        (
                          "cantAnalyzeComplexStateType",
                          [],
                          {
                            ptyp_loc: default_loc.contents,
                            ptyp_attributes: [],
                            ptyp_desc:
                              Ptyp_constr(
                                astHelperStrLidIdent([
                                  "pleaseProvideTheShapeOfStateManually",
                                ]),
                                [],
                              ),
                          },
                        ),
                      ],
                      Closed,
                    ),
                },
              ],
            ),
        },
        astHelperStrLidStr("state"),
      ),
    ]);
  /* drill deeeeeply into the function AST to get the shape of the return value */
  switch (initialStateDeclaration) {
  | {
      pcf_desc:
        Pcf_method((
          _,
          _,
          Cfk_concrete(
            _,
            {
              pexp_desc:
                Pexp_poly(
                  {
                    pexp_desc:
                      Pexp_fun(
                        _,
                        _,
                        _,
                        {pexp_desc: Pexp_constraint({pexp_desc: a}, _)},
                      ),
                  },
                  None,
                ),
            },
          ),
        )),
    } =>
    switch (innerMostExpr(a)) {
    | Some(returnExpression) =>
      let fields =
        returnExpression
        |> List.map((({txt}, {pexp_desc})) => {
             let label =
               switch (txt) {
               | Lident(txt) => txt
               | _ => "cannotConvertStateKeyOver"
               };
             Type.field(
               astHelperStrLidStr(label),
               {
                 ptyp_loc: default_loc.contents,
                 ptyp_attributes: [],
                 ptyp_desc:
                   Ptyp_constr(
                     astHelperStrLidIdent([
                       "pleaseFillTheTypeForThisKeyManually",
                     ]),
                     [],
                   ),
               },
             );
           });
      Str.type_([
        Type.mk(~kind=Ptype_record(fields), astHelperStrLidStr("state")),
      ]);
    | None => bailType
    }
  | _ =>
    /* getInitialState too complex; just generate a dummy */
    bailType
  };
};

let rec statementBlockMapper =
        (
          ~context,
          {Parser_flow.Ast.Statement.Block.body}: Parser_flow.Ast.Statement.Block.t,
        )
        : Parsetree.expression =>
  switch (body) {
  | [] => expUnit
  | bodyNotEmpty =>
    let bodyNotEmptyFlipped = List.rev(bodyNotEmpty);
    let lastItemReason =
      List.hd(bodyNotEmptyFlipped)
      |> statementMapper(~context={...context, terminalExpr: None});
    List.fold_left(
      (accumExp, statement) =>
        statementMapper(
          ~context={...context, terminalExpr: Some(accumExp)},
          statement,
        ),
      lastItemReason,
      List.tl(bodyNotEmptyFlipped),
    );
  }
and functionMapper =
    (
      ~context,
      ~returnType,
      {
        Parser_flow.Ast.Function.id,
        params: (params, restParam),
        body,
        expression,
        _,
      },
    ) => {
  open Parser_flow.Ast;
  /* functionMapper is currently the only one using context.addPropsAndStateDeclImmediately. It wraps around the
     function in props and state declaration. don't forget to unset `context.addPropsAndStateDeclImmediately`; */
  let bodyReason =
    switch (body) {
    | Function.BodyExpression(expression) =>
      expressionMapper(context, expression)
    | Function.BodyBlock((_, body)) => statementBlockMapper(context, body)
    };
  let wrapBodyInReturnType = body =>
    switch (returnType) {
    | None => body
    | Some(typeName) =>
      Exp.constraint_(
        body,
        Typ.constr(astHelperStrLidIdent([typeName]), []),
      )
    };
  switch (params) {
  | [] =>
    Exp.fun_(
      "",
      None,
      Pat.construct(astHelperStrLidIdent(~correct=false, ["()"]), None),
      wrapBodyInReturnType(bodyReason),
    )
  | [(_, first), ...rest] =>
    let partialResult =
      List.rev(rest)
      |> List.fold_left(
           (expr', (_, param)) =>
             switch (param) {
             | Pattern.Identifier({Pattern.Identifier.name: (_, name)}) =>
               Exp.fun_(
                 "",
                 None,
                 Pat.construct(astHelperStrLidIdent([name]), None),
                 expr',
               )
             | Pattern.Object(_)
             | Pattern.Array(_)
             | Pattern.Assignment(_)
             | Pattern.Expression(_) =>
               Exp.fun_(
                 "",
                 None,
                 Pat.var(astHelperStrLidStr("fixme")),
                 expr',
               )
             },
           bodyReason,
         );
    switch (first) {
    | Pattern.Identifier({Pattern.Identifier.name: (_, name)}) =>
      Exp.fun_(
        "",
        None,
        Pat.construct(astHelperStrLidIdent([name]), None),
        wrapBodyInReturnType(partialResult),
      )
    | _ =>
      Exp.fun_(
        "",
        None,
        Pat.var(astHelperStrLidStr("fixme")),
        wrapBodyInReturnType(partialResult),
      )
    };
  };
}
and literalMapper =
    (~isNegativeNumber=false, {Parser_flow.Ast.Literal.value, raw}) =>
  Parser_flow.Ast.Literal.(
    switch (value) {
    | String(s) => Exp.constant(Const_string(s, None))
    | Boolean(boolean) =>
      /* translate boolean into BuckleScript true/false. We can't translate to ocaml true/false bc these
         will compile, through BS, to number. *Very* dangerous to do interop this way and pass around
         numbers thinking you're holding true/false */
      /* boolean ? Exp. */
      Exp.ident(
        astHelperStrLidIdent(
          ~correct=false,
          ["Js", boolean ? "true_" : "false_"],
        ),
      )
    | Null =>
      Exp.ident(astHelperStrLidIdent(~correct=false, ["Js", "null"]))
    | Number(n) =>
      let intN = int_of_float(n);
      /* if it's an integer */
      if (float_of_int(intN) == n) {
        Exp.constant(Const_int(isNegativeNumber ? - intN : intN));
      } else {
        Exp.constant(
          Const_float(string_of_float(isNegativeNumber ? -. n : n)),
        );
      };
    | RegExp(_) => placeholder("regexPlaceholder")
    }
  )
and jsxElementMapper =
    (
      ~context,
      {
        Parser_flow.Ast.JSX.openingElement: (
          _,
          {Parser_flow.Ast.JSX.Opening.name, selfClosing, attributes},
        ),
        children,
        _,
      },
    ) =>
  Parser_flow.Ast.JSX.(
    switch (name) {
    | Identifier((_, {Identifier.name})) =>
      let jsxPropHasHyphen =
        attributes
        |> List.exists(attr =>
             switch (attr) {
             | Opening.Attribute((
                 _,
                 {
                   Attribute.name:
                     Attribute.Identifier((_, {Identifier.name})),
                 },
               )) =>
               String.contains(name, '-')
             | _ => false
             }
           );
      let childrenReact =
        children
        |> List.map(child => jsxChildMapper(~context, child))
        |> keepSome;
      let constructRecordOrLabels = f =>
        attributes
        |> List.map(attr =>
             switch (attr) {
             | Opening.Attribute((_, {Attribute.name, value})) =>
               /* JSX's <Foo checked /> is sugar for <Foo checked={true} />. What a waste */
               let valueReason =
                 switch (value) {
                 | None =>
                   Exp.ident(
                     astHelperStrLidIdent(~correct=false, ["Js", "true_"]),
                   )
                 | Some(Attribute.Literal(_, lit)) => literalMapper(lit)
                 | Some(
                     Attribute.ExpressionContainer(
                       _,
                       {ExpressionContainer.expression},
                     ),
                   ) =>
                   switch (expression) {
                   | ExpressionContainer.Expression(expr) =>
                     expressionMapper(~context, expr)
                   | ExpressionContainer.EmptyExpression(_) => expUnit
                   }
                 };
               switch (name) {
               | Attribute.Identifier((_, {Identifier.name})) => (
                   f(name),
                   valueReason,
                 )
               | Attribute.NamespacedName(_) => (
                   f("NamespacedName"),
                   placeholder("notImplementedYet"),
                 )
               };
             | Opening.SpreadAttribute(_) => (
                 f("spreadAttrbute"),
                 placeholder("notImplementedYet"),
               )
             }
           );
      if (jsxPropHasHyphen) {
        /* if there's a hyphen (e.g. aria-label) then we can't transform it into a jsx function label (invalid syntax). We'll
           have to take a shortcut and use a bs.obj instead */
        let jsObj =
          Exp.extension((
            astHelperStrLidStr("bs.obj"),
            PStr([
              Str.eval(
                Exp.record(
                  constructRecordOrLabels(name =>
                    astHelperStrLidIdent([name])
                  ),
                  None,
                ),
              ),
            ]),
          ));
        Exp.apply(
          Exp.ident(
            astHelperStrLidIdent(
              ~correct=false,
              ["ReactRe", "createElement"],
            ),
          ),
          [
            ("", Exp.ident(astHelperStrLidIdent(~correct=false, [name]))),
            ("", jsObj),
            ("", Exp.array(childrenReact)),
          ],
        );
      } else {
        let partialArguments =
          constructRecordOrLabels(name => correctIdentifier(name));
        /* add children */
        let arguments =
          partialArguments @ [("", listToListAst(childrenReact))];
        Exp.apply(
          ~attrs=[(astHelperStrLidStr(~correct=false, "JSX"), PStr([]))],
          Exp.ident(astHelperStrLidIdent(~correct=false, [name])),
          arguments,
        );
      };
    | MemberExpression((_, {MemberExpression._object, property})) =>
      placeholder("complexJSXYet")
    | NamespacedName(_) => placeholder("noNameSpaceJSXYet")
    }
  )
and jsxChildMapper = (~context, (_, child)) =>
  Parser_flow.Ast.JSX.(
    switch (child) {
    | Parser_flow.Ast.JSX.Element(element) =>
      Some(jsxElementMapper(~context, element))
    | Parser_flow.Ast.JSX.ExpressionContainer({
        ExpressionContainer.expression,
      }) =>
      switch (expression) {
      | ExpressionContainer.Expression(expr) =>
        Some(expressionMapper(~context, expr))
      | ExpressionContainer.EmptyExpression(_) => Some(expUnit)
      }
    | Parser_flow.Ast.JSX.Text({Text.value, _}) =>
      /* JS jsx is whitespace sensitive and scatters around "\n          " in the AST */
      let trimmedValue = String.trim(value);
      if (trimmedValue == "") {
        None;
      } else {
        Some(Exp.constant(Const_string(trimmedValue, None)));
      };
    }
  )
and objectMapper = (~context, {Parser_flow.Ast.Expression.Object.properties}) =>
  Parser_flow.Ast.Expression.Object.(
    Parser_flow.Ast.(
      switch (properties) {
      | [] =>
        Exp.extension((
          astHelperStrLidStr("bs.raw"),
          PStr([Str.eval(Exp.constant(Const_string("{}", None)))]),
        ))
      | properties =>
        Exp.extension((
          astHelperStrLidStr("bs.obj"),
          PStr([
            Str.eval(
              Exp.record(
                properties
                |> List.map(property =>
                     switch (property) {
                     | Property((_, {Property.key, value})) =>
                       let keyReason =
                         switch (key) {
                         | Property.Literal((_, {Literal.value: name})) =>
                           switch (name) {
                           | Literal.String(s) => [s]
                           | Literal.Boolean(b) => [string_of_bool(b)]
                           | Literal.Null => ["null"]
                           | Literal.Number(n) => [string_of_float(n)]
                           | Literal.RegExp(_) => [
                               "regexAsKeyNotImplementedYet",
                             ]
                           }
                         | Property.Identifier((_, name)) => [name]
                         | Property.Computed(_) => ["notThereYet"]
                         };
                       (
                         astHelperStrLidIdent(keyReason),
                         expressionMapper(~context, value),
                       );
                     | SpreadProperty(_) => (
                         astHelperStrLidIdent([
                           "objectSpreadNotImplementedYet",
                         ]),
                         placeholder("objectSpreadNotImplementedYet"),
                       )
                     }
                   ),
                None,
              ),
            ),
          ]),
        ))
      }
    )
  )
and memberMapper =
    (
      ~context,
      {
        Parser_flow.Ast.Expression.Member._object: (_, _object) as objectWrap,
        property,
        computed,
      },
    ) => {
  /* heuristics: if it's Foo.bar, transform into Foo.bar in ocaml (module property). If it's foo.bar,
     transform into foo##bar, which BuckleScript will pick up and compile (back) into dot. Will we reach
     a fixed point lol? */
  /* TODO: actually implement this */
  open Parser_flow.Ast;
  open Parser_flow.Ast.Expression;
  let defaultCase = () => {
    let left = expressionMapper(~context, objectWrap);
    switch (property) {
    | Member.PropertyExpression(
        (_, Literal({Literal.value: Literal.Number(n)})) as expr,
      ) =>
      /* foo[1] => foo.(1); */
      let intN = int_of_float(n);
      if (float_of_int(intN) == n) {
        Exp.apply(
          Exp.ident(astHelperStrLidIdent(~correct=false, ["Array", "get"])),
          [("", left), ("", Exp.constant(Const_int(intN)))],
        );
      } else {
        expressionMapper(~context, expr);
      };
    /* astexplorer flow says foo[bar], bar is an identifier. In reality this flow parses it as an expression */
    | Member.PropertyIdentifier((_, name)) =>
      switch (_object) {
      | Identifier((_, "reactDOM" | "ReactDOM")) =>
        /* ReactDOM.foo -> ReactDOMRe.foo */
        Exp.ident(
          astHelperStrLidIdent(~correct=false, ["ReactDOMRe", name]),
        )
      | _ =>
        /* foo.bar => foo##bar; */
        Exp.apply(
          Exp.ident(astHelperStrLidIdent(~correct=false, ["##"])),
          [("", left), ("", Exp.ident(astHelperStrLidIdent([name])))],
        )
      }
    | Member.PropertyExpression((_, expr) as exprWrap) =>
      switch (_object) {
      | Identifier((_, "reactDOM" | "ReactDOM")) =>
        let name =
          switch (property) {
          | Member.PropertyIdentifier((_, name)) => name
          | _ => "notSureWhatThisIs"
          };
        Exp.apply(
          Exp.ident(
            astHelperStrLidIdent(~correct=false, ["ReactDOMRe", name]),
          ),
          [("", expressionMapper(~context, exprWrap))],
        );
      | _ =>
        if (computed) {
          /* foo.[bar] => foo.(bar); treat as array */
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, ["Array", "get"]),
            ),
            [("", left), ("", expressionMapper(~context, exprWrap))],
          );
        } else {
          /* foo.bar => foo##bar; */
          Exp.apply(
            Exp.ident(astHelperStrLidIdent(~correct=false, ["##"])),
            [("", left), ("", expressionMapper(~context, exprWrap))],
          );
        }
      }
    };
  };
  switch (context.insideReactClass) {
  | InsidePropTypes =>
    switch (property) {
    | Member.PropertyIdentifier((_, name)) =>
      switch (name) {
      | "isRequired" =>
        Exp.apply(
          Exp.ident(
            astHelperStrLidIdent(
              ~correct=false,
              ["ReactRe", "PropTypes", name],
            ),
          ),
          [("", expressionMapper(~context, objectWrap))],
        )
      | "oneOfType"
      | "oneOf"
      | "objectOf"
      | "instanceOf"
      | "arrayOf"
      | "string"
      | "bool"
      | "number"
      | "node"
      | "symbol"
      | "any"
      | "element"
      | "func"
      | "shape" =>
        Exp.ident(
          astHelperStrLidIdent(
            ~correct=false,
            ["ReactRe", "PropTypes", name],
          ),
        )
      | "object" =>
        Exp.ident(
          astHelperStrLidIdent(
            ~correct=false,
            ["ReactRe", "PropTypes", "object_"],
          ),
        )
      | unrecognizedPropName => defaultCase()
      }
    | Member.PropertyExpression(expr) => expressionMapper(~context, expr)
    }
  | Yes(_) =>
    switch (_object, property) {
    | (This, Member.PropertyIdentifier((_, ("props" | "state") as name))) =>
      /* we turn `this.props` (assumed to be a react class) into just `props`. Same for `state`. */
      Exp.ident(astHelperStrLidIdent([name]))
    | _ => defaultCase()
    }
  | Nope => defaultCase()
  };
}
and statementMapper =
    (~context, (_, statement): Parser_flow.Ast.Statement.t)
    : Parsetree.expression =>
  Parser_flow.Ast.Statement.(
    Parser_flow.Ast.(
      switch (statement) {
      | VariableDeclaration({VariableDeclaration.declarations}) =>
        /* this is the part that transforms non-top-level var declarations list (in a single var
           declaration) from js to like, let with a tuple or something in reason */
        /* TODO: actually do this lol */
        let (
          _,
          {Statement.VariableDeclaration.Declarator.id: (_, id), init},
        ) =
          List.hd(declarations);
        /* check if it's a let foo = React.createClass */
        let (context, isReactClassDecl) =
          switch (init, id) {
          | (
              Some((
                _,
                Expression.Call({
                  Expression.Call.callee: (
                    _,
                    Expression.Member({
                      Expression.Member._object: (
                        _,
                        Expression.Identifier((_, "React")),
                      ),
                      property:
                        Expression.Member.PropertyIdentifier((
                          _,
                          "createClass",
                        )),
                      _,
                    }),
                  ),
                  arguments,
                }),
              )),
              Pattern.Identifier({Pattern.Identifier.name: (_, name)}),
            ) => (
              {...context, insideReactClass: Yes(name)},
              Some(arguments),
            )
          | _ => (context, None)
          };
        let expr =
          switch (init) {
          | None =>
            Exp.construct(
              astHelperStrLidIdent(~correct=false, ["None"]),
              None,
            )
          | Some(e) => expressionMapper(~context, e)
          };
        let innerMostExpr =
          switch (context.terminalExpr) {
          | None => expUnit
          | Some(expr) => expr
          };
        switch (id) {
        | Pattern.Identifier({Pattern.Identifier.name: (_, name)}) =>
          Exp.let_(
            Nonrecursive,
            [
              parseTreeValueBinding(
                ~pat=
                  Pat.var(
                    astHelperStrLidStr(
                      switch (isReactClassDecl) {
                      | None => name
                      | Some(_) => "comp"
                      },
                    ),
                  ),
                ~expr,
              ),
            ],
            innerMostExpr,
          )
        | Pattern.Object({Pattern.Object.properties}) =>
          switch (properties, init) {
          | (
              [
                Pattern.Object.Property((
                  _,
                  {
                    Pattern.Object.Property.key:
                      Pattern.Object.Property.Identifier((_, "PropTypes")),
                  },
                )),
              ],
              Some((_, Expression.Identifier((_, "React")))),
            ) =>
            /* if it's let {PropTypes} = React.PropTypes, then ignore it; we don't use it at the end. */
            innerMostExpr
          | _ =>
            Exp.let_(
              Nonrecursive,
              [
                parseTreeValueBinding(
                  ~pat=
                    Pat.constant(
                      Const_string("destructuringNotImplemented", None),
                    ),
                  ~expr,
                ),
              ],
              innerMostExpr,
            )
          }
        | Pattern.Array(_)
        | Pattern.Assignment(_)
        | Pattern.Expression(_) =>
          Exp.let_(
            Nonrecursive,
            [
              parseTreeValueBinding(
                ~pat=
                  Pat.constant(
                    Const_string("destructuringNotImplemented", None),
                  ),
                ~expr,
              ),
            ],
            innerMostExpr,
          )
        };
      | Return({Return.argument}) =>
        /* TODO: warn against early return */
        let result =
          switch (argument) {
          | None => expUnit
          | Some(expr) => expressionMapper(~context, expr)
          };
        switch (context.terminalExpr) {
        | None => result
        | Some(expr) => Exp.sequence(result, expr)
        };
      | Expression({Statement.Expression.expression}) =>
        switch (context.terminalExpr) {
        | None => expressionMapper(~context, expression)
        | Some(expr) =>
          Exp.sequence(expressionMapper(~context, expression), expr)
        }
      | If({If.test, consequent, alternate}) =>
        let result =
          Exp.ifthenelse(
            expressionMapper(~context, test),
            statementMapper(
              ~context={...context, terminalExpr: None},
              consequent,
            ),
            switch (alternate) {
            | None => None
            | Some(statement) =>
              Some(
                statementMapper(
                  ~context={...context, terminalExpr: None},
                  statement,
                ),
              )
            },
          );
        switch (context.terminalExpr) {
        | None => result
        | Some(expr) => Exp.sequence(result, expr)
        };
      | Block(body) => statementBlockMapper(~context, body)
      | FunctionDeclaration(functionWrap) =>
        let funcName =
          switch (functionWrap.Function.id) {
          | None => "thisNameShouldntAppearPleaseReport"
          | Some((_, name)) => name
          };
        let innerMostExpr =
          switch (context.terminalExpr) {
          | None => expUnit
          | Some(expr) => expr
          };
        Exp.let_(
          Nonrecursive,
          [
            parseTreeValueBinding(
              ~pat=Pat.var(astHelperStrLidStr(funcName)),
              ~expr=functionMapper(~context, ~returnType=None, functionWrap),
            ),
          ],
          innerMostExpr,
        );
      | Empty =>
        switch (context.terminalExpr) {
        | None => expUnit
        | Some(expr) => expr
        }
      | ClassDeclaration({
          id,
          Parser_flow.Ast.Class.body: (_, {Parser_flow.Ast.Class.Body.body}),
          superClass,
          _,
        }) =>
        switch (id, superClass) {
        | (
            Some((_, className)),
            Some((
              _,
              Parser_flow.Ast.Expression.Member({
                Parser_flow.Ast.Expression.Member._object: (
                  _,
                  Parser_flow.Ast.Expression.Identifier((_, "React")),
                ),
                property,
                _,
              }),
            )),
          ) =>
          let context = {...context, insideReactClass: Yes(className)};
          let hasDisplayNameAlready = ref(false);
          let createClassSpecForEs6Class =
            body
            /* filter out "props" since we already use propTypes */
            |> List.filter(property =>
                 switch (property) {
                 | Class.Body.Property((
                     _,
                     {
                       Class.Property.key:
                         Expression.Object.Property.Identifier((_, "props")),
                       value,
                       typeAnnotation,
                       static,
                     },
                   )) =>
                   false
                 | _ => true
                 }
               )
            |> List.map(property =>
                 Parser_flow.Ast.(
                   switch (property) {
                   | Class.Body.Method((
                       _,
                       {
                         Class.Method.kind,
                         key,
                         value: (_, value),
                         static,
                         _,
                       },
                     )) =>
                     switch (key) {
                     | Expression.Object.Property.Identifier((_, name)) =>
                       if (static) {
                         Cf.val_(
                           astHelperStrLidStr("staticMethod"),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             placeholder("notImplementedYet"),
                           ),
                         );
                       } else {
                         Cf.method(
                           astHelperStrLidStr(name),
                           Public,
                           Cfk_concrete(
                             Fresh,
                             Exp.poly(
                               functionMapper(
                                 ~context,
                                 ~returnType=None,
                                 value,
                               ),
                               None,
                             ),
                           ),
                         );
                       }
                     | _ =>
                       Cf.val_(
                         astHelperStrLidStr("complexClassPropKey"),
                         Immutable,
                         Cfk_concrete(
                           Fresh,
                           placeholder("notImplementedYet"),
                         ),
                       )
                     }
                   | Class.Body.Property((
                       _,
                       {Class.Property.key, value, typeAnnotation, static},
                     )) =>
                     switch (key) {
                     | Expression.Object.Property.Identifier((_, name)) =>
                       switch (name, static, value) {
                       | ("propTypes", true, Some(value)) =>
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(
                               ~context={
                                 ...context,
                                 insideReactClass: InsidePropTypes,
                               },
                               value,
                             ),
                           ),
                         )
                       | ("displayName", true, Some(value)) =>
                         hasDisplayNameAlready := true;
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(~context, value),
                           ),
                         );
                       | (_, true, _) =>
                         Cf.val_(
                           astHelperStrLidStr(
                             "staticPropertyOtherThanPropTypes",
                           ),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             placeholder("notImplementedYet"),
                           ),
                         )
                       | ("state", _, Some(value)) =>
                         Cf.method(
                           astHelperStrLidStr("getInitialState"),
                           Public,
                           Cfk_concrete(
                             Fresh,
                             Exp.poly(
                               Exp.fun_(
                                 "",
                                 None,
                                 Pat.construct(
                                   astHelperStrLidIdent(
                                     ~correct=false,
                                     ["()"],
                                   ),
                                   None,
                                 ),
                                 Exp.constraint_(
                                   expressionMapper(~context, value),
                                   Typ.constr(
                                     astHelperStrLidIdent(["state"]),
                                     [],
                                   ),
                                 ),
                               ),
                               None,
                             ),
                           ),
                         )
                       | (name, _, Some(value)) =>
                         switch (value) {
                         | (_, Expression.Function(f))
                         | (_, Expression.ArrowFunction(f)) =>
                           Cf.method(
                             astHelperStrLidStr(name),
                             Public,
                             Cfk_concrete(
                               Fresh,
                               Exp.poly(
                                 functionMapper(
                                   ~context,
                                   ~returnType=None,
                                   f,
                                 ),
                                 None,
                               ),
                             ),
                           )
                         | _ =>
                           Cf.val_(
                             astHelperStrLidStr(name),
                             Mutable,
                             Cfk_concrete(
                               Fresh,
                               expressionMapper(~context, value),
                             ),
                           )
                         }
                       | (name, _, None) =>
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Mutable,
                           Cfk_concrete(
                             Fresh,
                             Exp.ident(
                               astHelperStrLidIdent(
                                 ~correct=false,
                                 ["Js", "null"],
                               ),
                             ),
                           ),
                         )
                       }
                     | _ =>
                       Cf.val_(
                         astHelperStrLidStr("complexClassPropKey"),
                         Immutable,
                         Cfk_concrete(
                           Fresh,
                           placeholder("notImplementedYet"),
                         ),
                       )
                     }
                   }
                 )
               );
          let createClassSpecForEs6Class =
            hasDisplayNameAlready^ ?
              createClassSpecForEs6Class :
              [
                Cf.val_(
                  astHelperStrLidStr("displayName"),
                  Immutable,
                  Cfk_concrete(
                    Fresh,
                    Exp.constant(Const_string(className, None)),
                  ),
                ),
                ...createClassSpecForEs6Class,
              ];
          let createClassObj =
            Exp.object_(
              ~attrs=[(astHelperStrLidStr("bs"), PStr([]))],
              Cstr.mk(
                Pat.mk(Ppat_var(astHelperStrLidStr("this"))),
                createClassSpecForEs6Class,
              ),
            );
          let expr =
            Exp.apply(
              Exp.ident(
                astHelperStrLidIdent(
                  ~correct=false,
                  ["ReactRe", "createClass"],
                ),
              ),
              [("", createClassObj)],
            );
          let terminal =
            switch (context.terminalExpr) {
            | None => expUnit
            | Some(expr) => expr
            };
          Exp.let_(
            Nonrecursive,
            /* every react component must be called comp so that we could do Foo.comp on it for JSX */
            [
              parseTreeValueBinding(
                ~pat=Pat.var(astHelperStrLidStr("comp")),
                ~expr,
              ),
            ],
            terminal,
          );
        | _ => placeholder("GeneralClassTransformNotImplementedYet")
        }
      | ExportDefaultDeclaration({ExportDefaultDeclaration.declaration}) =>
        switch (declaration) {
        | ExportDefaultDeclaration.Declaration(decl) =>
          statementMapper(~context, decl)
        | ExportDefaultDeclaration.Expression(expr) =>
          expressionMapper(~context, expr)
        }
      | ExportNamedDeclaration({ExportNamedDeclaration.declaration}) =>
        switch (declaration) {
        | None => expUnit
        | Some(statement) => statementMapper(~context, statement)
        }
      | Labeled(_)
      | Break(_)
      | Continue(_)
      | With(_)
      | TypeAlias(_)
      | Switch(_)
      | Throw(_)
      | Try(_)
      | While(_)
      | DoWhile(_)
      | For(_)
      | ForIn(_)
      | ForOf(_)
      | Debugger
      | InterfaceDeclaration(_)
      | DeclareVariable(_)
      | DeclareFunction(_)
      | DeclareClass(_)
      | DeclareModule(_)
      | DeclareModuleExports(_)
      | DeclareExportDeclaration(_)
      | ImportDeclaration(_) =>
        switch (context.terminalExpr) {
        | None => placeholder("statementBail")
        | Some(expr) => Exp.sequence(placeholder("statementBail"), expr)
        }
      }
    )
  )
and expressionMapper =
    (~context, (_, expression): Parser_flow.Ast.Expression.t)
    : Parsetree.expression =>
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      switch (expression) {
      | Object(obj) => objectMapper(~context, obj)
      | ArrowFunction(functionWrap)
      | Function(functionWrap) =>
        functionMapper(~context, ~returnType=None, functionWrap)
      | Call({Call.callee: (_, callee) as calleeWrap, arguments}) =>
        let argumentsIntoReasonArguments = arguments =>
          arguments
          |> List.map(argument =>
               switch (argument) {
               | Expression(e) => expressionMapper(~context, e)
               | Spread((_, _)) =>
                 placeholder("argumentSpreadNotImplementedYet")
               }
             );
        let processArguments = arguments =>
          /* see Expression.Function above: */
          /* Js: () => 1 has 0 param. In reason, it has one param: unit. */
          switch (argumentsIntoReasonArguments(arguments)) {
          | [] => [("", expUnit)]
          | oneArgOrMore => oneArgOrMore |> List.map(arg => ("", arg))
          };
        switch (callee, context.insideReactClass, arguments) {
        | (
            Member({
              Member._object: (_, This),
              property: Member.PropertyIdentifier((_, "setState")),
              _,
            }),
            Yes(_),
            _,
          ) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, ["ReactRe", "setState"]),
            ),
            [
              ("", Exp.ident(astHelperStrLidIdent(["this"]))),
              ...processArguments(arguments),
            ],
          )
        | (
            Expression.Member({
              Expression.Member._object: (
                _,
                Expression.Identifier((_, "React")),
              ),
              property:
                Expression.Member.PropertyIdentifier((_, "createClass")),
              _,
            }),
            Yes(className),
            [Expression((_, Object({Object.properties})))],
          ) =>
          /* the context insideReactClass is already set by the variable declarator (search declarator)
             a level above */
          let hasDisplayNameAlready = ref(false);
          let createClassSpecForCreateClassDecl =
            properties
            |> List.map(property =>
                 Object.(
                   switch (property) {
                   | Property((
                       _,
                       {
                         Property.key: Property.Identifier((_, name)),
                         value: (_, value) as valueWrap,
                         kind: Property.Init,
                         _,
                       },
                     )) =>
                     switch (value) {
                     | Function(functionWrap)
                     | ArrowFunction(functionWrap) =>
                       Cf.method(
                         astHelperStrLidStr(name),
                         Public,
                         Cfk_concrete(
                           Fresh,
                           Exp.poly(
                             functionMapper(
                               ~context,
                               ~returnType=
                                 name == "getInitialState" ?
                                   Some("state") : None,
                               functionWrap,
                             ),
                             None,
                           ),
                         ),
                       )
                     | _ =>
                       switch (name) {
                       | "propTypes" =>
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(
                               ~context={
                                 ...context,
                                 insideReactClass: InsidePropTypes,
                               },
                               valueWrap,
                             ),
                           ),
                         )
                       | "mixins" =>
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(~context, valueWrap),
                           ),
                         )
                       | "displayName" =>
                         hasDisplayNameAlready := true;
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Immutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(~context, valueWrap),
                           ),
                         );
                       | name =>
                         Cf.val_(
                           astHelperStrLidStr(name),
                           Mutable,
                           Cfk_concrete(
                             Fresh,
                             expressionMapper(~context, valueWrap),
                           ),
                         )
                       }
                     }
                   | Property(_)
                   | SpreadProperty(_) =>
                     Cf.val_(
                       astHelperStrLidStr("notSureWhat"),
                       Immutable,
                       Cfk_concrete(
                         Fresh,
                         Exp.ident(astHelperStrLidIdent(["thisIs"])),
                       ),
                     )
                   }
                 )
               );
          let createClassSpecForCreateClassDecl =
            hasDisplayNameAlready^ ?
              createClassSpecForCreateClassDecl :
              [
                Cf.val_(
                  astHelperStrLidStr("displayName"),
                  Immutable,
                  Cfk_concrete(
                    Fresh,
                    Exp.constant(Const_string(className, None)),
                  ),
                ),
                ...createClassSpecForCreateClassDecl,
              ];
          let createClassObj =
            Exp.object_(
              ~attrs=[(astHelperStrLidStr("bs"), PStr([]))],
              Cstr.mk(
                Pat.mk(Ppat_var(astHelperStrLidStr("this"))),
                createClassSpecForCreateClassDecl,
              ),
            );
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(
                ~correct=false,
                ["ReactRe", "createClass"],
              ),
            ),
            [("", createClassObj)],
          );
        | (Identifier((_, "cx")), _, _) =>
          /* cx is treated differntly; facebook-specific */
          /* turns cx('a', 'b') into Cx.cxRe [|'a', 'b'|] */
          let argumentsIntoReasonArguments2 = arguments =>
            arguments
            |> List.map(argument =>
                 switch (argument) {
                 | Expression((_, Literal({Literal.value})) as expr) =>
                   expressionMapper(~context, expr)
                 | Expression(expr) =>
                   Exp.apply(
                     Exp.ident(
                       astHelperStrLidIdent(
                         ~correct=false,
                         ["Obj", "magic"],
                       ),
                     ),
                     [("", expressionMapper(~context, expr))],
                   )
                 | Spread(_) => placeholder("unregonizedSpreadInCx")
                 }
               );
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, ["CxRe", "cxRe"]),
            ),
            [("", Exp.array(argumentsIntoReasonArguments2(arguments)))],
          );
        | (Identifier((_, "joinClasses")), _, _) =>
          /* cx is treated differntly; facebook-specific */
          /* turns cx('a', 'b') into Cx.cxRe [|'a', 'b'|] */
          let argumentsIntoReasonArguments2 = arguments =>
            arguments
            |> List.map(argument =>
                 switch (argument) {
                 | Expression(expr) => expressionMapper(~context, expr)
                 | Spread(_) => placeholder("unregonizedSpreadInCx")
                 }
               );
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(
                ~correct=false,
                ["JoinClassesRe", "joinClasses"],
              ),
            ),
            [("", Exp.array(argumentsIntoReasonArguments2(arguments)))],
          );
        | (Identifier((_, "cssVar")), _, _) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, ["CssVarRe", "cssVarRe"]),
            ),
            processArguments(arguments),
          )
        | (Identifier((_, "fbt")), _, _) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, ["FbtRe", "fbtRe"]),
            ),
            processArguments(arguments),
          )
        | (Identifier((_, "invariant")), _, _) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(
                ~correct=false,
                ["InvariantRe", "invariant"],
              ),
            ),
            processArguments(arguments),
          )
        | (Identifier((_, "ix")), _, _) =>
          Exp.apply(
            Exp.ident(astHelperStrLidIdent(~correct=false, ["IxRe", "ix"])),
            processArguments(arguments),
          )
        | (_, _, _) =>
          Exp.apply(
            expressionMapper(~context, calleeWrap),
            processArguments(arguments),
          )
        };
      | Identifier((_, name)) => Exp.ident(astHelperStrLidIdent([name]))
      | Literal(lit) => literalMapper(lit)
      | Member(member) => memberMapper(~context, member)
      | This => Exp.ident(astHelperStrLidIdent(["this"]))
      | Logical({
          Logical.operator,
          left: leftWrap,
          right: (_, right) as rightWrap,
        }) =>
        switch (operator) {
        | Logical.Or =>
          Exp.apply(
            Exp.ident(astHelperStrLidIdent(~correct=false, ["||"])),
            [
              ("", expressionMapper(~context, leftWrap)),
              ("", expressionMapper(~context, rightWrap)),
            ],
          )
        | Logical.And =>
          /* common pattern: `show && <Foo />`. Transpile to `show ? <Foo /> : Js.null` */
          switch (right) {
          | JSXElement(_) =>
            Exp.match(
              expressionMapper(~context, leftWrap),
              [
                {
                  pc_lhs: Pat.construct(astHelperStrLidIdent(["true"]), None),
                  pc_guard: None,
                  pc_rhs: expressionMapper(~context, rightWrap),
                },
                {
                  pc_lhs:
                    Pat.construct(astHelperStrLidIdent(["false"]), None),
                  pc_guard: None,
                  pc_rhs:
                    Exp.ident(
                      astHelperStrLidIdent(~correct=false, ["Js", "null"]),
                    ),
                },
              ],
            )
          | _ =>
            Exp.apply(
              Exp.ident(astHelperStrLidIdent(~correct=false, ["&&"])),
              [
                ("", expressionMapper(~context, leftWrap)),
                ("", expressionMapper(~context, rightWrap)),
              ],
            )
          }
        }
      | JSXElement(element) => jsxElementMapper(~context, element)
      | Array({Array.elements}) =>
        elements
        |> List.map(element =>
             switch (element) {
             | None =>
               Exp.construct(
                 astHelperStrLidIdent(~correct=false, ["None"]),
                 None,
               )
             | Some(Expression(e)) => expressionMapper(~context, e)
             | Some(Spread((_, _))) =>
               placeholder("argumentSpreadNotImplementedYet")
             }
           )
        |> Exp.array
      | Binary({Binary.operator, left, right}) =>
        let operatorReason =
          switch (operator) {
          | Binary.Equal => "="
          | Binary.NotEqual => "!="
          | Binary.StrictEqual => "=="
          | Binary.StrictNotEqual => "!=="
          | Binary.LessThan => "<"
          | Binary.LessThanEqual => "<="
          | Binary.GreaterThan => ">"
          | Binary.GreaterThanEqual => ">="
          | Binary.LShift => "lsl"
          | Binary.RShift => "lsr"
          | Binary.RShift3 => "RShift3NotImplemented"
          /* TODO: integer/float */
          | Binary.Plus => "+"
          | Binary.Minus => "-"
          | Binary.Mult => "*"
          | Binary.Exp => "**"
          | Binary.Div => "/"
          | Binary.Mod => "mod"
          | Binary.BitOr => "lor"
          | Binary.Xor => "lxor"
          | Binary.BitAnd => "land"
          | Binary.In => "inNotImplemented"
          | Binary.Instanceof => "instanceOfNotImplemented"
          };
        switch (operator, left, right) {
        | (Binary.Equal, (_, Literal({Literal.value: Literal.Null})), _) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(
                ~correct=false,
                ["Js", "Null_undefined", "test"],
              ),
            ),
            [("", expressionMapper(~context, right))],
          )
        | (Binary.Equal, _, (_, Literal({Literal.value: Literal.Null}))) =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(
                ~correct=false,
                ["Js", "Null_undefined", "test"],
              ),
            ),
            [("", expressionMapper(~context, left))],
          )
        | _ =>
          Exp.apply(
            Exp.ident(
              astHelperStrLidIdent(~correct=false, [operatorReason]),
            ),
            [
              ("", expressionMapper(~context, left)),
              ("", expressionMapper(~context, right)),
            ],
          )
        };
      | Assignment({Assignment.operator, left: (_, left), right}) =>
        /* let innerMostExpr =
           switch context.terminalExpr {
           | None => expUnit
           | Some expr => expr
           }; */
        switch (left) {
        | Pattern.Expression((
            _,
            Member({
              Member._object: (_, Identifier((_, "module"))),
              property: Member.PropertyIdentifier((_, "exports")),
            }),
          )) =>
          /* module.exports is added naturally by BuckleScript. We don't need any translation from JS. */
          expUnit
        | _ =>
          /* TODO: this is redundant with what's above */
          let leftReason =
            switch (left) {
            | Pattern.Expression(expr) => expressionMapper(~context, expr)
            | Pattern.Identifier({Pattern.Identifier.name: (_, name)}) =>
              Exp.ident(astHelperStrLidIdent([name]))
            | Pattern.Object(_)
            | Pattern.Array(_)
            | Pattern.Assignment(_) => expMarker
            };
          Exp.apply(
            Exp.ident(astHelperStrLidIdent(~correct=false, ["#="])),
            [("", leftReason), ("", expressionMapper(~context, right))],
          );
        }
      | Unary({
          Unary.operator,
          prefix,
          argument: (_, argument) as argumentWrap,
        }) =>
        switch (operator) {
        | Unary.Not =>
          switch (argument) {
          | Unary({Unary.operator: Unary.Not, argument: innerArgument}) =>
            /* !! is a js idiom for casting to boolean */
            Exp.apply(
              Exp.ident(
                astHelperStrLidIdent(["pleaseWriteAIsTruthyFunction"]),
              ),
              [("", expressionMapper(~context, innerArgument))],
            )
          | _ =>
            Exp.apply(
              Exp.ident(astHelperStrLidIdent(["not"])),
              [("", expressionMapper(~context, argumentWrap))],
            )
          }
        | Unary.Minus =>
          switch (argument) {
          | Literal({Literal.value: Literal.Number(_)} as lit) =>
            /* !! is a js idiom for casting to boolean */
            literalMapper(~isNegativeNumber=true, lit)
          | _ =>
            Exp.apply(
              Exp.ident(astHelperStrLidIdent(["~-"])),
              [("", expressionMapper(~context, argumentWrap))],
            )
          }
        | Unary.Plus
        | Unary.BitNot
        | Unary.Typeof
        | Unary.Void
        | Unary.Delete
        | Unary.Await =>
          Exp.apply(
            Exp.ident(astHelperStrLidIdent(["unaryPlaceholder"])),
            [("", expressionMapper(~context, argumentWrap))],
          )
        }
      | Conditional({Conditional.test, consequent, alternate}) =>
        Exp.match(
          expressionMapper(~context, test),
          [
            {
              pc_lhs: Pat.construct(astHelperStrLidIdent(["true"]), None),
              pc_guard: None,
              pc_rhs: expressionMapper(~context, consequent),
            },
            {
              pc_lhs: Pat.construct(astHelperStrLidIdent(["false"]), None),
              pc_guard: None,
              pc_rhs: expressionMapper(~context, alternate),
            },
          ],
        )
      | Sequence(_)
      | Update(_)
      | New(_)
      | Yield(_)
      | Comprehension(_)
      | Generator(_)
      | TemplateLiteral(_)
      | TaggedTemplate(_)
      | Class(_)
      | TypeCast(_)
      | Super
      | MetaProperty(_) =>
        Exp.ident(astHelperStrLidIdent(["expressionPlaceholder"]))
      }
    )
  );

/* top level output format are slightly different than expressions (they have to be `structure_item`s). We'd
   like to reuse the statementMapper's logic as much as possible though; so we use it, destructure to
   get what we want, then re-wrap for top level output. */
let topStatementsMapper = statementWrap => {
  let {pexp_desc, _} =
    statementMapper(
      ~context={
        terminalExpr: None,
        insideReactClass: Nope,
        reactClassSpecRandomProps: [],
      },
      statementWrap,
    );
  switch (pexp_desc) {
  | Pexp_let(_, valueBindings, {pexp_desc, _}) =>
    /* get some propTypes, generate externals and type decls, e.g. type props = ... */
    let extraDeclarations =
      switch (valueBindings) {
      | [
          {
            pvb_expr: {
              pexp_desc:
                Pexp_apply(
                  {
                    pexp_desc:
                      Pexp_ident({
                        txt: Ldot(Lident("ReactRe"), "createClass"),
                      }),
                    _,
                  },
                  [(_, {pexp_desc: Pexp_object({pcstr_fields})})],
                ),
            },
          },
        ] =>
        let propTypes =
          pcstr_fields
          |> List.filter(({pcf_desc}) =>
               switch (pcf_desc) {
               | Pcf_val(({txt: "propTypes"}, _, _)) => true
               | _ => false
               }
             );
        let extraDeclarationsFromProps =
          switch (propTypes) {
          | [
              {
                pcf_desc:
                  Pcf_val((
                    _,
                    _,
                    Cfk_concrete(
                      _,
                      {
                        pexp_desc:
                          Pexp_extension((
                            _,
                            PStr([
                              {
                                pstr_desc:
                                  Pstr_eval(
                                    {pexp_desc: Pexp_record(fields, _)},
                                    _,
                                  ),
                              },
                            ]),
                          )),
                      },
                    ),
                  )),
              },
            ] =>
            switch (fields) {
            | [] => []
            | fields =>
              let (propsType, externalType) = propTypesToActualTypes(fields);
              [propsType, externalType];
            }
          | _ => []
          };
        /* same for state type */
        let getInitialState =
          pcstr_fields
          |> List.filter(({pcf_desc}) =>
               switch (pcf_desc) {
               | Pcf_method(({txt: "getInitialState"}, _, _)) => true
               | _ => false
               }
             );
        let extraDeclarationsFromState =
          switch (getInitialState) {
          | [initialStateDeclaration] => [
              attemptToGenerateStateRecord(initialStateDeclaration),
            ]
          | _ => []
          };
        extraDeclarationsFromProps @ extraDeclarationsFromState;
      | [] => [Str.eval(expMarker)]
      | _ => []
      };
    let baseDeclarations =
      switch (valueBindings) {
      /* strip `require` calls */
      | [
          {
            pvb_expr: {
              pexp_desc:
                Pexp_apply(
                  {pexp_desc: Pexp_ident({txt: Lident("require")})},
                  _,
                ),
            },
          },
        ] =>
        []
      | _ => [Str.value(Nonrecursive, valueBindings)]
      };
    extraDeclarations @ baseDeclarations;
  | Pexp_constant(a) =>
    switch (a) {
    | Const_string("use strict", _) => []
    | _ => [Str.eval(Exp.constant(a))]
    }
  | Pexp_ifthenelse(cond, consequent, alternate) => [
      Str.eval(Exp.ifthenelse(cond, consequent, alternate)),
    ]
  | Pexp_fun(expr, _, argument, body) => [
      Str.value(
        Nonrecursive,
        [parseTreeValueBinding(~pat=argument, ~expr=body)],
      ),
    ]
  | Pexp_function(a) => [
      Str.value(
        Nonrecursive,
        [
          {
            pvb_pat: Pat.var(astHelperStrLidStr("topPlaceholderMe")),
            pvb_expr: expUnit,
            pvb_attributes: [],
            pvb_loc: default_loc.contents,
          },
        ],
      ),
    ]
  | Pexp_ident(ident) => [Str.eval(Exp.ident(ident))]
  | Pexp_apply(expr, lst) => [Str.eval(Exp.apply(expr, lst))]
  | Pexp_construct(_, _) => []
  | Pexp_letmodule(ident, modExpr, _) => [
      Str.module_({
        pmb_name: ident,
        pmb_expr: modExpr,
        pmb_attributes: [],
        pmb_loc: default_loc.contents,
      }),
    ]
  | Pexp_record(_, _)
  | Pexp_match(_, _)
  | Pexp_try(_, _)
  | Pexp_tuple(_)
  | Pexp_variant(_, _)
  | Pexp_field(_, _)
  | Pexp_setfield(_, _, _)
  | Pexp_array(_)
  | Pexp_sequence(_, _)
  | Pexp_while(_, _)
  | Pexp_for(_, _, _, _, _)
  | Pexp_constraint(_, _)
  | Pexp_coerce(_, _, _)
  | Pexp_send(_, _)
  | Pexp_new(_)
  | Pexp_setinstvar(_, _)
  | Pexp_override(_)
  | Pexp_assert(_)
  | Pexp_lazy(_)
  | Pexp_poly(_, _)
  | Pexp_object(_)
  | Pexp_newtype(_, _)
  | Pexp_pack(_)
  | Pexp_open(_, _, _)
  | Pexp_extension(_) => [
      Str.mk(
        Pstr_value(
          Nonrecursive,
          [
            {
              pvb_pat: Pat.var(astHelperStrLidStr("topPlaceholder")),
              pvb_expr: expUnit,
              pvb_attributes: [],
              pvb_loc: default_loc.contents,
            },
          ],
        ),
      ),
    ]
  };
};

let cat = filename => {
  let ic = open_in_bin(filename);
  let len = in_channel_length(ic);
  let buf = Buffer.create(len);
  Buffer.add_channel(buf, ic, len);
  let content = Buffer.contents(buf);
  close_in(ic);
  content;
};

let () =
  if (Array.length(Sys.argv) != 2) {
    raise(
      Invalid_argument(
        "Please provide as argument the JS file to convert over.",
      ),
    );
  } else {
    let file = Sys.argv[1];
    let parse_options =
      Some(
        Parser_env.{
          /***
           * Always parse ES proposal syntax. The user-facing config option to
           * ignore/warn/enable them is handled during inference so that a clean error
           * can be surfaced (rather than a more cryptic parse error).
           */
          esproposal_class_instance_fields: true,
          esproposal_class_static_fields: true,
          esproposal_decorators: true,
          esproposal_export_star_as: true,
          types: true,
          use_strict: false,
        },
      );
    let content = cat(file);
    let (ast, _) =
      Parser_flow.program_file(
        ~fail=false,
        ~parse_options,
        content,
        Some(Loc.SourceFile(file)),
      );
    let (_, statements, _) = ast;
    output_string(stdout, Config.ast_impl_magic_number);
    output_value(stdout, file);
    let result: Parsetree.structure =
      statements
      |> List.map(statementWrap => topStatementsMapper(statementWrap))
      |> List.concat;
    output_value(stdout, result);
  };
