/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

let astHelperStrLid a => {loc: default_loc.contents, txt: a};

let expUnit = Exp.construct (astHelperStrLid (Lident "()")) None;

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

/* let flattenMemberProperties asd =>
   Parser_flow.Ast.Expression.(
     switch asd {
     | Member.PropertyIdentifier _ => (??)
     | Member.PropertyExpression _ => (??)
     }
   ); */
type context = {
  terminalExpr: option Parsetree.expression,
  /* insideReactCreateClass: bool, */
  insidePropTypes: bool
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
  ignore restParam;
  let bodyReason =
    switch body {
    | Function.BodyExpression expression => expressionMapper context::context expression
    | Function.BodyBlock (_, body) => statementBlockMapper context::context body
    };
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
    | Null => Exp.ident (astHelperStrLid (Ldot (Ldot (Lident "Js") "Null") "Empty"))
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
    {Parser_flow.Ast.Expression.Member._object: (_, _object) as objectWrap, property, _} =>
  /* heuristics: if it's Foo.bar, transform into Foo.bar in ocaml (module property). If it's foo.bar,
     transform into foo##bar, which BuckleScript will pick up and compile (back) into dot. Will we reach
     a fixed point lol? */
  /* TODO: actually implement this */
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      if context.insidePropTypes {
        switch property {
        | Member.PropertyIdentifier (_, {Identifier.name: name}) =>
          switch name {
          | "isRequired" =>
            Exp.apply
              (Exp.ident (astHelperStrLid (Ldot (Ldot (Lident "React") "PropTypes") "isRequired")))
              [("", expressionMapper context::context objectWrap)]
          | actualPropName =>
            Exp.ident (astHelperStrLid (Ldot (Ldot (Lident "React") "PropTypes") actualPropName))
          }
        | Member.PropertyExpression expr => expressionMapper context::context expr
        }
      } else {
        let propertyReason =
          switch property {
          | Member.PropertyIdentifier (_, {Identifier.name: name, _}) =>
            Exp.ident (astHelperStrLid (Lident name))
          | Member.PropertyExpression expr => expressionMapper context::context expr
          };
        let left = expressionMapper context::context objectWrap;
        Exp.apply (Exp.ident (astHelperStrLid (Lident "##"))) [("", left), ("", propertyReason)]
      }
    )
  )
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
      | Parser_flow.Ast.Statement.VariableDeclaration {
          VariableDeclaration.declarations: declarations,
          kind
        } =>
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
      | Parser_flow.Ast.Statement.Return {Return.argument: argument} =>
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
      | Parser_flow.Ast.Statement.Expression {Expression.expression: expression} =>
        switch context.terminalExpr {
        | None => expressionMapper context::context expression
        | Some expr => Exp.sequence (expressionMapper context::context expression) expr
        }
      | Parser_flow.Ast.Statement.If {If.test: test, consequent, alternate} =>
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
      | Parser_flow.Ast.Statement.Block body => statementBlockMapper context::context body
      | Parser_flow.Ast.Statement.FunctionDeclaration functionWrap =>
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
      | Parser_flow.Ast.Statement.Empty =>
        switch context.terminalExpr {
        | None => expUnit
        | Some expr => expr
        }
      | Parser_flow.Ast.Statement.Labeled _
      | Parser_flow.Ast.Statement.Break _
      | Parser_flow.Ast.Statement.Continue _
      | Parser_flow.Ast.Statement.With _
      | Parser_flow.Ast.Statement.TypeAlias _
      | Parser_flow.Ast.Statement.Switch _
      | Parser_flow.Ast.Statement.Throw _
      | Parser_flow.Ast.Statement.Try _
      | Parser_flow.Ast.Statement.While _
      | Parser_flow.Ast.Statement.DoWhile _
      | Parser_flow.Ast.Statement.For _
      | Parser_flow.Ast.Statement.ForIn _
      | Parser_flow.Ast.Statement.ForOf _
      | Parser_flow.Ast.Statement.Let _
      | Parser_flow.Ast.Statement.Debugger
      | Parser_flow.Ast.Statement.ClassDeclaration _
      | Parser_flow.Ast.Statement.InterfaceDeclaration _
      | Parser_flow.Ast.Statement.DeclareVariable _
      | Parser_flow.Ast.Statement.DeclareFunction _
      | Parser_flow.Ast.Statement.DeclareClass _
      | Parser_flow.Ast.Statement.DeclareModule _
      | Parser_flow.Ast.Statement.DeclareModuleExports _
      | Parser_flow.Ast.Statement.DeclareExportDeclaration _
      | Parser_flow.Ast.Statement.ExportDeclaration _
      | Parser_flow.Ast.Statement.ImportDeclaration _ =>
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
      | Parser_flow.Ast.Expression.Object {Object.properties: properties} =>
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
      | Parser_flow.Ast.Expression.ArrowFunction functionWrap
      | Parser_flow.Ast.Expression.Function functionWrap =>
        functionMapper context::context functionWrap
      | Parser_flow.Ast.Expression.Call {Call.callee: (_, callee) as calleeWrap, arguments} =>
        switch (callee, arguments) {
        | (
            Member {
              Member._object: (_, Identifier (_, {Identifier.name: "React", _})),
              property: Member.PropertyIdentifier (_, {Identifier.name: "createClass", _}),
              _
            },
            [Expression (_, Object {Object.properties: properties})]
          ) =>
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
                                  context::{...context, insidePropTypes: true} valueWrap
                              )
                          )
                      /* (Cfk_concrete Fresh (reactPropTypesMapper property)) */
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
        | (_, arguments) =>
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
          let argumentsReason =
            switch arguments {
            | [] => [("", expUnit)]
            | oneArgOrMore => argumentsReason
            };
          Exp.apply (expressionMapper context::context calleeWrap) argumentsReason
        }
      | Parser_flow.Ast.Expression.Identifier (_, {Identifier.name: name}) =>
        Exp.ident (astHelperStrLid (Lident name))
      | Parser_flow.Ast.Expression.Literal lit => literalMapper lit
      | Parser_flow.Ast.Expression.Member member => memberMapper context::context member
      | Parser_flow.Ast.Expression.This => Exp.ident (astHelperStrLid (Lident "this"))
      | Parser_flow.Ast.Expression.Logical {Logical.operator: operator, left, right} =>
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
      | Parser_flow.Ast.Expression.JSXElement element => jsxElementMapper context::context element
      | Parser_flow.Ast.Expression.Array {Array.elements: elements} =>
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
      | Parser_flow.Ast.Expression.Sequence _
      | Parser_flow.Ast.Expression.Unary _
      | Parser_flow.Ast.Expression.Binary _
      | Parser_flow.Ast.Expression.Assignment _
      | Parser_flow.Ast.Expression.Update _
      | Parser_flow.Ast.Expression.Conditional _
      | Parser_flow.Ast.Expression.New _
      | Parser_flow.Ast.Expression.Yield _
      | Parser_flow.Ast.Expression.Comprehension _
      | Parser_flow.Ast.Expression.Generator _
      | Parser_flow.Ast.Expression.Let _
      | Parser_flow.Ast.Expression.TemplateLiteral _
      | Parser_flow.Ast.Expression.TaggedTemplate _
      | Parser_flow.Ast.Expression.Class _
      | Parser_flow.Ast.Expression.TypeCast _
      | Parser_flow.Ast.Expression.MetaProperty _ =>
        Exp.constant (Const_string "expressionPlaceholder" None)
      }
    )
  );

/* top level output format are slightly different than expressions (they have to be `structure_item`s). We'd
   like to reuse the statementMapper's logic as much as possible though; so we use it, destructure to
   get what we want, then re-wrap for top level output. */
let topStatementsMapper statementWrap => {
  let {pexp_desc, _} =
    statementMapper context::{terminalExpr: None, insidePropTypes: false} statementWrap;
  switch pexp_desc {
  | Pexp_let _ valueBindings {pexp_desc, _} => Str.value Nonrecursive valueBindings
  | Pexp_constant a => Str.eval (Exp.constant a)
  | Pexp_ifthenelse cond consequent alternate =>
    Str.eval (Exp.ifthenelse cond consequent alternate)
  | Pexp_fun expr _ argument body =>
    Str.value Nonrecursive [parseTreeValueBinding pat::argument expr::body]
  | Pexp_function a =>
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
  | Pexp_ident ident => Str.eval (Exp.ident ident)
  | Pexp_apply expr lst => Str.eval (Exp.apply expr lst)
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
  | Pexp_extension _ =>
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
    statements |> List.map (fun statementWrap => topStatementsMapper statementWrap);
  output_value stdout result
};
