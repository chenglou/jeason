/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

/* helpers */
let astHelperStrLid a => {loc: default_loc.contents, txt: a};

let expUnit = Exp.construct (astHelperStrLid (Lident "()")) None;

/* using this as a convenient placeholder output, for checking whether I've matched the js ast correctly */
let expMarker = Exp.ident (astHelperStrLid (Lident "marker"));

let parseTreeValueBinding pat::pat expr::expr => {
  pvb_pat: pat,
  pvb_expr: expr,
  pvb_attributes: [],
  pvb_loc: default_loc.contents
};

let keepSome lst =>
  lst |>
  List.filter (
    fun a =>
      switch a {
      | None => false
      | _ => true
      }
  ) |>
  List.map (
    fun a =>
      switch a {
      | None => assert false
      | Some a => a
      }
  );

let listToListAst lst => {
  let nullList = Exp.construct (astHelperStrLid (Lident "[]")) None;
  /* we transform js array to BS array, except in the case of jsx which takes a reason list instead */
  switch lst {
  | [] => nullList
  | oneItemOrMore =>
    List.rev oneItemOrMore |>
    List.fold_left
      (
        fun accumExp expr =>
          Exp.construct (astHelperStrLid (Lident "::")) (Some (Exp.tuple [expr, accumExp]))
      )
      nullList
  }
};

type context = {
  terminalExpr: option Parsetree.expression,
  insideReactCreateClass: bool,
  insidePropTypes: bool,
  mutable reactClassSpecRandomProps: list (list string),
  mutable reactClassSpecPropTypes: list (list (string, Parsetree.expression))
};

let rec convertPropTypeType asd =>
  switch asd {
  | Pexp_apply {pexp_desc: Pexp_ident {txt: Ldot (Ldot (Lident "ReactRe") "PropTypes") prop}} lst =>
    switch prop {
    | "string" => Ptyp_constr {loc: default_loc.contents, txt: Lident "string"} []
    | "bool" => Ptyp_constr {loc: default_loc.contents, txt: Lident "bool"} []
    | "number" => Ptyp_constr {loc: default_loc.contents, txt: Lident "number"} []
    | "object_" => Ptyp_constr {loc: default_loc.contents, txt: Lident "object_"} []
    | "symbol" => Ptyp_constr {loc: default_loc.contents, txt: Lident "symbol"} []
    | "any" => Ptyp_constr {loc: default_loc.contents, txt: Lident "any"} []
    | "oneOfType" => Ptyp_constr {loc: default_loc.contents, txt: Lident "oneOfType"} []
    | "oneOf" => Ptyp_constr {loc: default_loc.contents, txt: Lident "oneOf"} []
    | "element" => Ptyp_constr {loc: default_loc.contents, txt: Lident "element"} []
    | "func" => Ptyp_constr {loc: default_loc.contents, txt: Lident "func"} []
    | "objectOf" => Ptyp_constr {loc: default_loc.contents, txt: Lident "objectOf"} []
    | "arrayOf" => Ptyp_constr {loc: default_loc.contents, txt: Lident "arrayOf"} []
    | "instanceOf" => Ptyp_constr {loc: default_loc.contents, txt: Lident "instanceOf"} []
    | "shape" => Ptyp_constr {loc: default_loc.contents, txt: Lident "shape"} []
    | "isRequired" => Ptyp_constr {loc: default_loc.contents, txt: Lident "isRequired"} []
    | _ => Ptyp_constr {loc: default_loc.contents, txt: Lident "unrecognizedPropType"} []
    }
  | _ => Ptyp_constr {loc: default_loc.contents, txt: Lident "unrecognizedPropType"} []
  };

let propTypesShit fields => {
  /* reference: go from

     let propTypes = {
       "inner": PropTypes.isRequired PropTypes.number,
       "something": PropTypes.string,
       "children": PropTypes.element
     };

     to:

     type props =
     Js.t <
       inner : int,
       something : Js.null_undefined string,
       children : Js.null_undefined React.reactElement
     >;

     external props : inner::int => something::Js.null_undefined string? => unit => 'reactJsProps = "" [@@bs.obj]; */
  let convertedFields =
    fields |>
    List.map (
      fun ({txt}, {pexp_desc}) =>
        switch txt {
        | Lident propName =>
          switch pexp_desc {
          | Pexp_apply
              {pexp_desc: Pexp_ident {txt: Ldot (Ldot (Lident "ReactRe") "PropTypes") prop}} _ as asd => (
              propName,
              [],
              {
                ptyp_loc: default_loc.contents,
                ptyp_attributes: [],
                ptyp_desc: convertPropTypeType asd
              }
            )
          /* Exp.constant (Const_string prop None) */
          | _ => (
              "cannotGenerateType",
              [],
              {
                ptyp_loc: default_loc.contents,
                ptyp_attributes: [],
                ptyp_desc:
                  Ptyp_constr {loc: default_loc.contents, txt: Lident "ForThisFieldOfPropTypes"} []
              }
            )
          }
        | _ => (
            "cannotGenerateType",
            [],
            {
              ptyp_loc: default_loc.contents,
              ptyp_attributes: [],
              ptyp_desc:
                Ptyp_constr
                  {loc: default_loc.contents, txt: Lident "forComplexPropTypesObjectKey"} []
            }
          )
        }
    );
  let objType = Str.type_ [
    Type.mk
      kind::Ptype_abstract
      priv::Public
      manifest::{
        ptyp_loc: default_loc.contents,
        ptyp_attributes: [],
        ptyp_desc:
          Ptyp_constr
            {loc: default_loc.contents, txt: Ldot (Lident "Js") "t"}
            [
              {
                ptyp_loc: default_loc.contents,
                ptyp_attributes: [],
                ptyp_desc: Ptyp_object convertedFields Closed
              }
            ]
      }
      (astHelperStrLid "props")
  ];
  (objType, expMarker)
};

let rec statementBlockMapper
        context::context
        ({Parser_flow.Ast.Statement.Block.body: body}: Parser_flow.Ast.Statement.Block.t)
        :Parsetree.expression =>
  switch body {
  | [] => expUnit
  | bodyNotEmpty =>
    let bodyNotEmptyFlipped = List.rev bodyNotEmpty;
    let lastItemReason =
      List.hd bodyNotEmptyFlipped |> statementMapper context::{...context, terminalExpr: None};
    List.fold_left
      (
        fun accumExp statement =>
          statementMapper context::{...context, terminalExpr: Some accumExp} statement
      )
      lastItemReason
      (List.tl bodyNotEmptyFlipped)
  }
and functionMapper
    context::context
    {Parser_flow.Ast.Function.id: id, params: (params, restParam), body, expression, _} => {
  open Parser_flow.Ast;
  let bodyReason =
    switch body {
    | Function.BodyExpression expression => expressionMapper context::context expression
    | Function.BodyBlock (_, body) => statementBlockMapper context::context body
    };
  /* let bodyReason =
     context.insideReactCreateClass ?
       {
         let expr =
           Exp.constraint_
             (
               Exp.apply
                 (Exp.ident (astHelperStrLid (Ldot (Lident "ReactRe") "getState")))
                 [("", Exp.ident (astHelperStrLid (Lident "this")))]
             )
             (Typ.constr (astHelperStrLid (Lident "state")) []);
         Exp.let_
           Nonrecursive
           [parseTreeValueBinding pat::(Pat.var (astHelperStrLid "state")) expr::expr]
           bodyReason
       } :
       bodyReason; */
  let partialOrFullResult =
    List.rev params |>
    List.fold_left
      (
        fun expr' (_, param) =>
          switch param {
          | Pattern.Identifier (_, {Identifier.name: name, _}) =>
            Exp.fun_ "" None (Pat.construct (astHelperStrLid (Lident name)) None) expr'
          | _ => Exp.fun_ "" None (Pat.var (astHelperStrLid "fixme")) expr'
          }
      )
      bodyReason;
  /* Js: () => 1 has 0 param. In reason, it has one param: unit. */
  switch params {
  | [] => Exp.fun_ "" None (Pat.construct (astHelperStrLid (Lident "()")) None) partialOrFullResult
  | oneParamOrMore => partialOrFullResult
  }
}
and literalMapper {Parser_flow.Ast.Literal.value: value, raw} =>
  Parser_flow.Ast.Literal.(
    switch value {
    | String s => Exp.constant (Const_string s None)
    | Boolean boolean =>
      /* translate boolean into BuckleScript true/false. We can't translate to ocaml true/false bc these
         will compile, through BS, to number. *Very* dangerous to do interop this way and pass around
         numbers thinking you're holding true/false */
      /* boolean ? Exp. */
      Exp.ident (astHelperStrLid (Ldot (Lident "Js") (boolean ? "true_" : "false")))
    | Null => Exp.ident (astHelperStrLid (Ldot (Lident "Js") "null"))
    | Number n =>
      let intN = int_of_float n;
      if (float_of_int intN == n) {
        Exp.constant (Const_int intN)
      } else {
        Exp.constant (Const_float (string_of_float n))
      }
    | RegExp _ => Exp.constant (Const_string "regexPlaceholder" None)
    }
  )
and jsxElementMapper
    context::context
    {
      Parser_flow.Ast.JSX.openingElement: (
        _,
        {Parser_flow.Ast.JSX.Opening.name: name, selfClosing, attributes}
      ),
      children,
      _
    } =>
  Parser_flow.Ast.JSX.(
    switch name {
    | Identifier (_, {Identifier.name: name}) =>
      let partialArguments =
        attributes |>
        List.map (
          fun attr =>
            switch attr {
            | Opening.Attribute (_, {Attribute.name: name, value}) =>
              /* JSX's <Foo checked /> is sugar for <Foo checked={true} />. What a waste */
              let valueReason =
                switch value {
                | None => Exp.ident (astHelperStrLid (Ldot (Lident "Js") "true_"))
                | Some (Attribute.Literal _ lit) => literalMapper lit
                | Some (
                    Attribute.ExpressionContainer _ {ExpressionContainer.expression: expression}
                  ) =>
                  switch expression {
                  | ExpressionContainer.Expression expr => expressionMapper context::context expr
                  | ExpressionContainer.EmptyExpression _ => expUnit
                  }
                };
              switch name {
              | Attribute.Identifier (_, {Identifier.name: name}) => (name, valueReason)
              | Attribute.NamespacedName _ => (
                  "NamespacedName",
                  Exp.constant (Const_string "notImeplementedYet" None)
                )
              }
            | Opening.SpreadAttribute _ => (
                "spreadAttrbute",
                Exp.constant (Const_string "notImeplementedYet" None)
              )
            }
        );
      /* add children */
      let lastArgument =
        children |> List.map (fun child => jsxChildMapper context::context child) |> keepSome;
      let arguments = partialArguments @ [("", listToListAst lastArgument)];
      Exp.apply
        attrs::[(astHelperStrLid "JSX", PStr [])]
        (Exp.ident (astHelperStrLid (Lident name)))
        arguments
    | MemberExpression (_, {MemberExpression._object: _object, property}) =>
      Exp.constant (Const_string "complexJSXYet" None)
    | NamespacedName _ => Exp.constant (Const_string "noNameSpaceJSXYet" None)
    }
  )
and jsxChildMapper context::context (_, child) =>
  Parser_flow.Ast.JSX.(
    switch child {
    | Parser_flow.Ast.JSX.Element element => Some (jsxElementMapper context::context element)
    | Parser_flow.Ast.JSX.ExpressionContainer {ExpressionContainer.expression: expression} =>
      switch expression {
      | ExpressionContainer.Expression expr => Some (expressionMapper context::context expr)
      | ExpressionContainer.EmptyExpression _ => Some expUnit
      }
    | Parser_flow.Ast.JSX.Text {Text.value: value, _} =>
      /* JS jsx is whitespace sensitive and scatters around "\n          " in the AST */
      let trimmedValue = String.trim value;
      if (trimmedValue == "") {
        None
      } else {
        Some (Exp.constant (Const_string value None))
      }
    }
  )
and memberMapper
    context::context
    {Parser_flow.Ast.Expression.Member._object: (_, _object) as objectWrap, property, _} => {
  /* heuristics: if it's Foo.bar, transform into Foo.bar in ocaml (module property). If it's foo.bar,
     transform into foo##bar, which BuckleScript will pick up and compile (back) into dot. Will we reach
     a fixed point lol? */
  /* TODO: actually implement this */
  open Parser_flow.Ast;
  open Parser_flow.Ast.Expression;
  let defaultCase () => {
    let propertyReason =
      switch property {
      | Member.PropertyIdentifier (_, {Identifier.name: name, _}) =>
        Exp.ident (astHelperStrLid (Lident name))
      | Member.PropertyExpression expr => expressionMapper context::context expr
      };
    let left = expressionMapper context::context objectWrap;
    Exp.apply (Exp.ident (astHelperStrLid (Lident "##"))) [("", left), ("", propertyReason)]
  };
  if context.insidePropTypes {
    switch property {
    | Member.PropertyIdentifier (_, {Identifier.name: name}) =>
      switch name {
      | "isRequired" =>
        Exp.apply
          (Exp.ident (astHelperStrLid (Ldot (Ldot (Lident "ReactRe") "PropTypes") "isRequired")))
          [("", expressionMapper context::context objectWrap)]
      | actualPropName =>
        Exp.ident (astHelperStrLid (Ldot (Ldot (Lident "ReactRe") "PropTypes") actualPropName))
      }
    | Member.PropertyExpression expr => expressionMapper context::context expr
    }
  } else if
    context.insideReactCreateClass {
    switch (_object, property) {
    | (This, Member.PropertyIdentifier (_, {Identifier.name: "props"})) =>
      Exp.ident (astHelperStrLid (Lident "props"))
    | _ => defaultCase ()
    }
  } else {
    defaultCase ()
  }
}
and reactPropTypesMemberMapper context::context property =>
  /* TODO: use this */
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.Object.(
      switch property {
      | Property (
          _,
          {
            Property.key: Property.Identifier (_, {Identifier.name: name}),
            value: (_, Expression.Object {properties}),
            kind,
            _method,
            shorthand
          }
        ) =>
        Exp.extension (
          astHelperStrLid "bs.obj",
          PStr [
            Str.eval (
              Exp.record
                (
                  properties |>
                  List.map (
                    fun property =>
                      switch property {
                      | Property (_, {Property.key: key, value, kind, _method, _}) =>
                        ignore kind;
                        ignore _method;
                        let keyReason =
                          switch key {
                          | Property.Identifier (_, {Identifier.name: name, _}) => Lident name
                          | Property.Literal _
                          | Property.Computed _ => Lident "notThereYet"
                          };
                        (astHelperStrLid keyReason, expressionMapper context::context value)
                      | SpreadProperty _ => (
                          astHelperStrLid (Lident "objectSpreadNotImplementedYet"),
                          Exp.constant (Const_string "objectSpreadNotImplementedYet" None)
                        )
                      }
                  )
                )
                None
            )
          ]
        )
      | Property _ => Exp.constant (Const_string "propTypesReceivedWeirdFormat" None)
      | SpreadProperty _ => Exp.constant (Const_string "woahSpreadInPropTypesPleaseInline" None)
      }
    )
  )
and statementMapper
    context::context
    ((_, statement): Parser_flow.Ast.Statement.t)
    :Parsetree.expression =>
  Parser_flow.Ast.(
    Parser_flow.Ast.Statement.(
      switch statement {
      | VariableDeclaration {VariableDeclaration.declarations: declarations, kind} =>
        /* this is the part that transforms non-top-level var declarations list (in a single var
           declaration) from js to like, let with a tuple or something in reason */
        /* TODO: actually do this lol */
        ignore kind;
        let (_, {Statement.VariableDeclaration.Declarator.id: (_, id), init}) = List.hd declarations;
        let expr =
          switch init {
          | None => Exp.construct (astHelperStrLid (Lident "None")) None
          | Some e => expressionMapper context::context e
          };
        let innerMostExpr =
          switch context.terminalExpr {
          | None => expUnit
          | Some expr => expr
          };
        switch id {
        | Pattern.Identifier (_, {Identifier.name: name, _}) =>
          Exp.let_
            Nonrecursive
            [parseTreeValueBinding pat::(Pat.var (astHelperStrLid name)) expr::expr]
            innerMostExpr
        | Pattern.Object _
        | Pattern.Array _
        | Pattern.Assignment _
        | Pattern.Expression _ =>
          Exp.constant (Const_string "variationDeclOtherCasesNotImplemented" None)
        }
      | Return {Return.argument: argument} =>
        /* TODO: warn against early return */
        let result =
          switch argument {
          | None => expUnit
          | Some expr => expressionMapper context::context expr
          };
        switch context.terminalExpr {
        | None => result
        | Some expr => Exp.sequence result expr
        }
      | Expression {Expression.expression: expression} =>
        switch context.terminalExpr {
        | None => expressionMapper context::context expression
        | Some expr => Exp.sequence (expressionMapper context::context expression) expr
        }
      | If {If.test: test, consequent, alternate} =>
        let result =
          Exp.ifthenelse
            (expressionMapper context::context test)
            (statementMapper context::{...context, terminalExpr: None} consequent)
            (
              switch alternate {
              | None => None
              | Some statement =>
                Some (statementMapper context::{...context, terminalExpr: None} statement)
              }
            );
        switch context.terminalExpr {
        | None => result
        | Some expr => Exp.sequence result expr
        }
      | Block body => statementBlockMapper context::context body
      | FunctionDeclaration functionWrap =>
        let funcName =
          switch functionWrap.Function.id {
          | None => "thisNameShouldntAppearPleaseReport"
          | Some (_, {Identifier.name: name, _}) => name
          };
        let innerMostExpr =
          switch context.terminalExpr {
          | None => expUnit
          | Some expr => expr
          };
        Exp.let_
          Nonrecursive
          [
            parseTreeValueBinding
              pat::(Pat.var (astHelperStrLid funcName))
              expr::(functionMapper context::context functionWrap)
          ]
          innerMostExpr
      | Empty =>
        switch context.terminalExpr {
        | None => expUnit
        | Some expr => expr
        }
      | ClassDeclaration {
          id,
          Parser_flow.Ast.Class.body: (_, {Parser_flow.Ast.Class.Body.body: body}),
          superClass,
          _
        } =>
        switch (id, superClass) {
        | (
            Some (_, {Identifier.name: className}),
            Some (
              _,
              Parser_flow.Ast.Expression.Member {
                Parser_flow.Ast.Expression.Member._object: (
                  _,
                  Parser_flow.Ast.Expression.Identifier (_, {Identifier.name: "React"})
                ),
                property,
                _
              }
            )
          ) =>
          let context = {...context, insideReactCreateClass: true};
          let createClassSpec =
            body |>
            List.map (
              fun property =>
                Parser_flow.Ast.(
                  switch property {
                  | Class.Body.Method (
                      _,
                      {Class.Method.kind: kind, key, value: (_, value), static, _}
                    ) =>
                    switch key {
                    | Expression.Object.Property.Identifier (_, {Identifier.name: name, _}) =>
                      switch (name, static) {
                      | (name, false) =>
                        Cf.method_
                          (astHelperStrLid name)
                          Public
                          (
                            Cfk_concrete
                              Fresh (Exp.poly (functionMapper context::context value) None)
                          )
                      | (name, true) =>
                        Cf.val_
                          (astHelperStrLid "staticMethod")
                          Immutable
                          (Cfk_concrete Fresh (Exp.constant (Const_string "NotImplemented" None)))
                      }
                    | _ =>
                      Cf.val_
                        (astHelperStrLid "ComplexClassPropKey")
                        Immutable
                        (Cfk_concrete Fresh (Exp.constant (Const_string "NotImplemented" None)))
                    }
                  | Class.Body.Property (
                      _,
                      {Class.Property.key: key, value, typeAnnotation, static}
                    ) =>
                    switch key {
                    | Expression.Object.Property.Identifier (_, {Identifier.name: name, _}) =>
                      switch (name, static, value) {
                      | ("propTypes", true, Some value) =>
                        Cf.val_
                          (astHelperStrLid name)
                          Immutable
                          (
                            Cfk_concrete
                              Fresh
                              (expressionMapper context::{...context, insidePropTypes: true} value)
                          )
                      | ("displayName", true, Some value) =>
                        Cf.val_
                          (astHelperStrLid name)
                          Immutable
                          (Cfk_concrete Fresh (expressionMapper context::context value))
                      | (_, true, _) =>
                        Cf.val_
                          (astHelperStrLid "staticPropertyOtherThanPropTypes")
                          Immutable
                          (Cfk_concrete Fresh (Exp.constant (Const_string "NotImplemented" None)))
                      | ("state", _, Some value) =>
                        Cf.method_
                          (astHelperStrLid "getInitialState")
                          Public
                          (
                            Cfk_concrete
                              Fresh
                              (
                                Exp.poly
                                  (
                                    Exp.fun_
                                      ""
                                      None
                                      (Pat.construct (astHelperStrLid (Lident "()")) None)
                                      (expressionMapper context::context value)
                                  )
                                  None
                              )
                          )
                      | (name, _, Some value) =>
                        switch value {
                        | (_, Expression.Function f)
                        | (_, Expression.ArrowFunction f) =>
                          Cf.method_
                            (astHelperStrLid name)
                            Public
                            (
                              Cfk_concrete Fresh (Exp.poly (functionMapper context::context f) None)
                            )
                        | _ =>
                          Cf.val_
                            (astHelperStrLid name)
                            Mutable
                            (Cfk_concrete Fresh (expressionMapper context::context value))
                        }
                      | (name, _, None) =>
                        Cf.val_
                          (astHelperStrLid name)
                          Mutable
                          (
                            Cfk_concrete
                              Fresh (Exp.ident (astHelperStrLid (Ldot (Lident "Js") "null")))
                          )
                      }
                    | _ =>
                      Cf.val_
                        (astHelperStrLid "ComplexClassPropKey")
                        Immutable
                        (Cfk_concrete Fresh (Exp.constant (Const_string "NotImplemented" None)))
                    }
                  }
                )
            );
          let createClassObj =
            Exp.object_
              attrs::[(astHelperStrLid "bs", PStr [])]
              (Cstr.mk (Pat.mk (Ppat_var (astHelperStrLid "this"))) createClassSpec);
          let expr =
            Exp.apply
              (Exp.ident (astHelperStrLid (Ldot (Lident "ReactRe") "createClass")))
              [("", createClassObj)];
          let terminal =
            switch context.terminalExpr {
            | None => expUnit
            | Some expr => expr
            };
          Exp.let_
            Nonrecursive
            [parseTreeValueBinding pat::(Pat.var (astHelperStrLid className)) expr::expr]
            terminal
        | _ => Exp.constant (Const_string "GeneralClassTransformNotImplementedYet" None)
        }
      | Labeled _
      | Break _
      | Continue _
      | With _
      | TypeAlias _
      | Switch _
      | Throw _
      | Try _
      | While _
      | DoWhile _
      | For _
      | ForIn _
      | ForOf _
      | Let _
      | Debugger
      | InterfaceDeclaration _
      | DeclareVariable _
      | DeclareFunction _
      | DeclareClass _
      | DeclareModule _
      | DeclareModuleExports _
      | DeclareExportDeclaration _
      | ExportDeclaration _
      | ImportDeclaration _ =>
        switch context.terminalExpr {
        | None => Exp.constant (Const_string "statementBail" None)
        | Some expr => Exp.sequence (Exp.constant (Const_string "statementBail" None)) expr
        }
      }
    )
  )
and expressionMapper
    context::context
    ((_, expression): Parser_flow.Ast.Expression.t)
    :Parsetree.expression =>
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      switch expression {
      | Object {Object.properties: properties} =>
        Exp.extension (
          astHelperStrLid "bs.obj",
          PStr [
            Str.eval (
              Exp.record
                (
                  properties |>
                  List.map (
                    fun property =>
                      switch property {
                      | Object.Property (_, {Object.Property.key: key, value, kind, _method, _}) =>
                        ignore kind;
                        ignore _method;
                        let keyReason =
                          switch key {
                          | Object.Property.Identifier (_, {Identifier.name: name, _}) =>
                            Lident name
                          | Object.Property.Literal _
                          | Object.Property.Computed _ => Lident "notThereYet"
                          };
                        (astHelperStrLid keyReason, expressionMapper context::context value)
                      | Object.SpreadProperty _ => (
                          astHelperStrLid (Lident "objectSpreadNotImplementedYet"),
                          Exp.constant (Const_string "objectSpreadNotImplementedYet" None)
                        )
                      }
                  )
                )
                None
            )
          ]
        )
      | ArrowFunction functionWrap
      | Function functionWrap => functionMapper context::context functionWrap
      | Call {Call.callee: (_, callee) as calleeWrap, arguments} =>
        let processArguments arguments => {
          let argumentsReason =
            arguments |>
            List.map (
              fun argument =>
                switch argument {
                | Expression e => ("", expressionMapper context::context e)
                | Spread (_, _) => (
                    "",
                    Exp.constant (Const_string "argumentSpreadNotImplementedYet" None)
                  )
                }
            );
          /* see Expression.Function above: */
          /* Js: () => 1 has 0 param. In reason, it has one param: unit. */
          switch arguments {
          | [] => [("", expUnit)]
          | oneArgOrMore => argumentsReason
          }
        };
        switch (callee, arguments) {
        | (
            Member {
              Member._object: (_, Identifier (_, {Identifier.name: "React", _})),
              property: Member.PropertyIdentifier (_, {Identifier.name: "createClass", _}),
              _
            },
            [Expression (_, Object {Object.properties: properties})]
          ) =>
          let context = {...context, insideReactCreateClass: true};
          let createClassSpec =
            properties |>
            List.map (
              fun property =>
                Object.(
                  switch property {
                  | Property (
                      _,
                      {
                        Property.key: Property.Identifier (_, {Identifier.name: name, _}),
                        value: (_, value) as valueWrap,
                        kind: Property.Init,
                        _
                      }
                    ) =>
                    switch value {
                    | Function _
                    | ArrowFunction _ =>
                      Cf.method_
                        (astHelperStrLid name)
                        Public
                        /* TODO: might not be able to recurse. Might need to be binding specific */
                        (
                          Cfk_concrete
                            Fresh (Exp.poly (expressionMapper context::context valueWrap) None)
                        )
                    | _ =>
                      switch name {
                      | "propTypes" =>
                        Cf.val_
                          (astHelperStrLid name)
                          Immutable
                          (
                            Cfk_concrete
                              Fresh
                              (
                                expressionMapper
                                  context::{
                                    ...context,
                                    insidePropTypes: true,
                                    reactClassSpecPropTypes: [
                                      [],
                                      ...context.reactClassSpecPropTypes
                                    ]
                                  }
                                  valueWrap
                              )
                          )
                      | "displayName" =>
                        Cf.val_
                          (astHelperStrLid name)
                          Immutable
                          (Cfk_concrete Fresh (expressionMapper context::context valueWrap))
                      | name =>
                        Cf.val_
                          (astHelperStrLid name)
                          Mutable
                          (Cfk_concrete Fresh (expressionMapper context::context valueWrap))
                      }
                    }
                  | Property _
                  | SpreadProperty _ =>
                    Cf.val_
                      (astHelperStrLid "notSureWhat")
                      Immutable
                      (Cfk_concrete Fresh (Exp.ident (astHelperStrLid (Lident "thisIs"))))
                  }
                )
            );
          let createClassObj =
            Exp.object_
              attrs::[(astHelperStrLid "bs", PStr [])]
              (Cstr.mk (Pat.mk (Ppat_var (astHelperStrLid "this"))) createClassSpec);
          Exp.apply
            (Exp.ident (astHelperStrLid (Ldot (Lident "ReactRe") "createClass")))
            [("", createClassObj)]
        | (
            Member {
              Member._object: (_, This),
              /* property: Member.PropertyIdentifier (_, {Identifier.name: "setState", _}), */
              property: Member.PropertyIdentifier (_, {Identifier.name: "setState", _}),
              _
            },
            arguments
          )
            when context.insideReactCreateClass =>
          Exp.apply
            (Exp.ident (astHelperStrLid (Ldot (Lident "ReactRe") "setState")))
            [("", Exp.ident (astHelperStrLid (Lident "this"))), ...processArguments arguments]
        | (caller, arguments) =>
          Exp.apply (expressionMapper context::context calleeWrap) (processArguments arguments)
        }
      | Identifier (_, {Identifier.name: name}) => Exp.ident (astHelperStrLid (Lident name))
      | Literal lit => literalMapper lit
      | Member member => memberMapper context::context member
      | This => Exp.ident (astHelperStrLid (Lident "this"))
      | Logical {Logical.operator: operator, left, right} =>
        let operatorReason =
          switch operator {
          | Logical.Or => "||"
          | Logical.And => "&&"
          };
        Exp.apply
          (Exp.ident (astHelperStrLid (Lident operatorReason)))
          [
            ("", expressionMapper context::context left),
            ("", expressionMapper context::context right)
          ]
      | JSXElement element => jsxElementMapper context::context element
      | Array {Array.elements: elements} =>
        elements |>
        List.map (
          fun element =>
            switch element {
            | None => Exp.construct (astHelperStrLid (Lident "None")) None
            | Some (Expression e) => expressionMapper context::context e
            | Some (Spread (_, _)) =>
              Exp.constant (Const_string "argumentSpreadNotImplementedYet" None)
            }
        ) |> Exp.array
      | Binary {Binary.operator: operator, left, right} =>
        let operatorReason =
          switch operator {
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
        Exp.apply
          (Exp.ident (astHelperStrLid (Lident operatorReason)))
          [
            ("", expressionMapper context::context left),
            ("", expressionMapper context::context right)
          ]
      | Sequence _
      | Unary _
      | Assignment _
      | Update _
      | Conditional _
      | New _
      | Yield _
      | Comprehension _
      | Generator _
      | Let _
      | TemplateLiteral _
      | TaggedTemplate _
      | Class _
      | TypeCast _
      | MetaProperty _ => Exp.constant (Const_string "expressionPlaceholder" None)
      }
    )
  );

/* top level output format are slightly different than expressions (they have to be `structure_item`s). We'd
   like to reuse the statementMapper's logic as much as possible though; so we use it, destructure to
   get what we want, then re-wrap for top level output. */
let topStatementsMapper statementWrap => {
  let {pexp_desc, _} =
    statementMapper
      context::{
        terminalExpr: None,
        insidePropTypes: false,
        insideReactCreateClass: false,
        reactClassSpecRandomProps: [],
        reactClassSpecPropTypes: []
      }
      statementWrap;
  switch pexp_desc {
  | Pexp_let _ valueBindings {pexp_desc, _} =>
    /* get some propTypes, generate externals and type decls, e.g. type props = ... */
    switch valueBindings {
    | [
        {
          pvb_expr: {
            pexp_desc:
              Pexp_apply
                {pexp_desc: Pexp_ident {txt: Ldot (Lident "ReactRe") "createClass"}, _}
                [(_, {pexp_desc: Pexp_object {pcstr_fields}})]
          }
        }
      ] =>
      let propTypes =
        pcstr_fields |>
        List.filter (
          fun {pcf_desc} =>
            switch pcf_desc {
            | Pcf_val ({txt: "propTypes"}, _, _) => true
            | _ => false
            }
        );
      switch propTypes {
      | [
          {
            pcf_desc:
              Pcf_val (
                _,
                _,
                Cfk_concrete
                  _
                  {
                    pexp_desc:
                      Pexp_extension (
                        _,
                        PStr [{pstr_desc: Pstr_eval {pexp_desc: Pexp_record fields _} _}]
                      )
                  }
              )
          }
        ] =>
        switch fields {
        | [] => [Str.value Nonrecursive valueBindings]
        | fields =>
          let (a, b) = propTypesShit fields;
          [a, Str.eval b, Str.value Nonrecursive valueBindings]
        }
      | _ => [Str.value Nonrecursive valueBindings]
      }
    | _ => [Str.eval expUnit]
    }
  | Pexp_constant a => [Str.eval (Exp.constant a)]
  | Pexp_ifthenelse cond consequent alternate => [
      Str.eval (Exp.ifthenelse cond consequent alternate)
    ]
  | Pexp_fun expr _ argument body => [
      Str.value Nonrecursive [parseTreeValueBinding pat::argument expr::body]
    ]
  | Pexp_function a => [
      Str.value
        Nonrecursive
        [
          {
            pvb_pat: Pat.var (astHelperStrLid "topPlaceholderMe"),
            pvb_expr: expUnit,
            pvb_attributes: [],
            pvb_loc: default_loc.contents
          }
        ]
    ]
  | Pexp_ident ident => [Str.eval (Exp.ident ident)]
  | Pexp_apply expr lst => [Str.eval (Exp.apply expr lst)]
  | Pexp_record _ _
  | Pexp_match _ _
  | Pexp_try _ _
  | Pexp_tuple _
  | Pexp_construct _ _
  | Pexp_variant _ _
  | Pexp_field _ _
  | Pexp_setfield _ _ _
  | Pexp_array _
  | Pexp_sequence _ _
  | Pexp_while _ _
  | Pexp_for _ _ _ _ _
  | Pexp_constraint _ _
  | Pexp_coerce _ _ _
  | Pexp_send _ _
  | Pexp_new _
  | Pexp_setinstvar _ _
  | Pexp_override _
  | Pexp_letmodule _ _ _
  | Pexp_assert _
  | Pexp_lazy _
  | Pexp_poly _ _
  | Pexp_object _
  | Pexp_newtype _ _
  | Pexp_pack _
  | Pexp_open _ _ _
  | Pexp_extension _ => [
      Str.mk (
        Pstr_value
          Nonrecursive
          [
            {
              pvb_pat: Pat.var (astHelperStrLid "topPlaceholder"),
              pvb_expr: expUnit,
              pvb_attributes: [],
              pvb_loc: default_loc.contents
            }
          ]
      )
    ]
  }
};

let () = {
  let parse_options =
    Some Parser_env.{
           /**
            * Always parse ES proposal syntax. The user-facing config option to
            * ignore/warn/enable them is handled during inference so that a clean error
            * can be surfaced (rather than a more cryptic parse error).
            */
           esproposal_class_instance_fields: true,
           esproposal_class_static_fields: true,
           esproposal_decorators: true,
           esproposal_export_star_as: true,
           types: true,
           use_strict: false
         };
  let file = "test.js";
  let content = Sys_utils.cat file;
  let (ast, _) =
    Parser_flow.program_file
      fail::false parse_options::parse_options content (Some (Loc.SourceFile file));
  let (_, statements, _) = ast;
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout file;
  let result: Parsetree.structure =
    statements |> List.map (fun statementWrap => topStatementsMapper statementWrap) |> List.concat;
  output_value stdout result
};
