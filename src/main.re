/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

let expUnit = Exp.construct {loc: default_loc.contents, txt: Lident "()"} None;

let astHelperStr a => {loc: default_loc.contents, txt: a};

let astHelperLid (a: Longident.t) => {loc: default_loc.contents, txt: a};

let parseTreeValueBinding pat::pat expr::expr => {
  pvb_pat: pat,
  pvb_expr: expr,
  pvb_attributes: [],
  pvb_loc: default_loc.contents
};

let rec statementBlockMapper
        ({Parser_flow.Ast.Statement.Block.body: body}: Parser_flow.Ast.Statement.Block.t)
        :Parsetree.expression =>
  switch body {
  | [] => expUnit
  | bodyNotEmpty =>
    let bodyNotEmptyFlipped = List.rev bodyNotEmpty;
    let lastItemReason = List.hd bodyNotEmptyFlipped |> statementMapper terminalExpr::None;
    List.fold_left
      (fun accumExp statement => statementMapper terminalExpr::(Some accumExp) statement)
      lastItemReason
      (List.tl bodyNotEmptyFlipped)
  }
and functionMapper
    {Parser_flow.Ast.Function.id: id, params: (params, restParam), body, expression, _} => {
  open Parser_flow.Ast;
  ignore restParam;
  let bodyReason =
    switch body {
    | Function.BodyExpression expression => expressionMapper expression
    | Function.BodyBlock (_, body) => statementBlockMapper body
    };
  let partialOrFullResult =
    List.rev params |>
    List.fold_left
      (
        fun expr' (_, param) =>
          switch param {
          | Pattern.Identifier (_, {Identifier.name: name, _}) =>
            Exp.fun_ "" None (Pat.construct (astHelperLid (Lident name)) None) expr'
          | _ => Exp.fun_ "" None (Pat.var (astHelperStr "fixme")) expr'
          }
      )
      bodyReason;
  /* Js: () => 1 has 0 param. In reason, it has one param: unit. */
  switch params {
  | [] => Exp.fun_ "" None (Pat.construct (astHelperLid (Lident "()")) None) partialOrFullResult
  | oneParamOrMore => partialOrFullResult
  }
}
and statementMapper
    terminalExpr::terminalExpr
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
          | None => Exp.construct (astHelperLid (Lident "None")) None
          | Some e => expressionMapper e
          };
        let innerMostExpr =
          switch terminalExpr {
          | None => expUnit
          | Some expr => expr
          };
        switch id {
        | Pattern.Identifier (_, {Identifier.name: name, _}) =>
          Exp.let_
            Nonrecursive
            [parseTreeValueBinding pat::(Pat.var (astHelperStr name)) expr::expr]
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
          | Some expr => expressionMapper expr
          };
        switch terminalExpr {
        | None => result
        | Some expr => Exp.sequence result expr
        }
      | Parser_flow.Ast.Statement.Expression {Expression.expression: expression} =>
        switch terminalExpr {
        | None => expressionMapper expression
        | Some expr => Exp.sequence (expressionMapper expression) expr
        }
      | Parser_flow.Ast.Statement.If {If.test: test, consequent, alternate} =>
        let result =
          Exp.ifthenelse
            (expressionMapper test)
            (statementMapper terminalExpr::None consequent)
            (
              switch alternate {
              | None => None
              | Some statement => Some (statementMapper terminalExpr::None statement)
              }
            );
        switch terminalExpr {
        | None => result
        | Some expr => Exp.sequence result expr
        }
      | Parser_flow.Ast.Statement.Block body => statementBlockMapper body
      | Parser_flow.Ast.Statement.FunctionDeclaration functionWrap =>
        let funcName =
          switch functionWrap.Function.id {
          | None => "thisNameShouldntAppearPleaseReport"
          | Some (_, {Identifier.name: name, _}) => name
          };
        let innerMostExpr =
          switch terminalExpr {
          | None => expUnit
          | Some expr => expr
          };
        Exp.let_
          Nonrecursive
          [
            parseTreeValueBinding
              pat::(Pat.var (astHelperStr funcName)) expr::(functionMapper functionWrap)
          ]
          innerMostExpr
      | Parser_flow.Ast.Statement.Empty =>
        switch terminalExpr {
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
        switch terminalExpr {
        | None => Exp.constant (Const_string "statementBail" None)
        | Some expr => Exp.sequence (Exp.constant (Const_string "statementBail" None)) expr
        }
      }
    )
  )
and expressionMapper ((_, expression): Parser_flow.Ast.Expression.t) :Parsetree.expression =>
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      switch expression {
      | Parser_flow.Ast.Expression.Object {Object.properties: properties} =>
        Exp.extension (
          {loc: default_loc.contents, txt: "bs.obj"},
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
                        (astHelperLid keyReason, expressionMapper value)
                      | Object.SpreadProperty _ => (
                          astHelperLid (Lident "objectSpreadNotImplementedYet"),
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
      | Parser_flow.Ast.Expression.Function functionWrap => functionMapper functionWrap
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
                        (astHelperStr name)
                        Public
                        /* TODO: might not be able to recurse. Might need to be binding specific */
                        (Cfk_concrete Fresh (Exp.poly (expressionMapper valueWrap) None))
                    | _ =>
                      Cf.val_
                        {loc: default_loc.contents, txt: name}
                        Immutable
                        /* TODO: might not be able to recurse. Might need to be binding specific */
                        (Cfk_concrete Fresh (expressionMapper valueWrap))
                    }
                  | _ =>
                    Cf.val_
                      {loc: default_loc.contents, txt: "notSureWhat"}
                      Immutable
                      (
                        Cfk_concrete
                          Fresh (Exp.ident {loc: default_loc.contents, txt: Lident "thisIs"})
                      )
                  }
                )
            );
          let createClassObj =
            Exp.object_
              attrs::[({loc: default_loc.contents, txt: "bs"}, PStr [])]
              (
                Cstr.mk (Pat.mk (Ppat_var {loc: default_loc.contents, txt: "this"})) createClassSpec
              );
          Exp.apply
            (Exp.ident {loc: default_loc.contents, txt: Ldot (Lident "ReactRe") "createClass"})
            [("", createClassObj)]
        | (_, arguments) =>
          let argumentsReason =
            arguments |>
            List.map (
              fun argument =>
                switch argument {
                | Expression e => ("", expressionMapper e)
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
          Exp.apply (expressionMapper calleeWrap) argumentsReason
        }
      | Parser_flow.Ast.Expression.Identifier (_, {Identifier.name: name}) =>
        Exp.ident (astHelperLid (Lident name))
      | Parser_flow.Ast.Expression.Literal {Literal.value: value, raw} =>
        switch value {
        | Literal.String s => Exp.constant (Const_string s None)
        | Literal.Boolean boolean =>
          /* translate boolean into BuckleScript true/false. We can't translate to ocaml true/false bc these
             will compile, through BS, to number. *Very* dangerous to do interop this way and pass around
             numbers thinking you're holding true/false */
          /* boolean ? Exp. */
          Exp.ident (astHelperLid (Lident (boolean ? "Js.true_" : "Js.false")))
        | Literal.Null => Exp.ident (astHelperLid (Lident "Js.Null.Empty"))
        | Literal.Number n =>
          let intN = int_of_float n;
          if (float_of_int intN == n) {
            Exp.constant (Const_int intN)
          } else {
            Exp.constant (Const_float (string_of_float n))
          }
        | Literal.RegExp _ => Exp.constant (Const_string "regexPlaceholder" None)
        }
      | Parser_flow.Ast.Expression.Member {Member._object: _object, property, _} =>
        /* compile js dot access into ##, which BuckleScript will pick up and compile (back) into dot. Will we
           reach a fixed point lol? */
        let propertyReason =
          switch property {
          | Member.PropertyIdentifier (_, {Identifier.name: name, _}) =>
            Exp.ident (astHelperLid (Lident name))
          | Member.PropertyExpression expr => expressionMapper expr
          };
        Exp.apply
          (Exp.ident (astHelperLid (Lident "##")))
          [("", expressionMapper _object), ("", propertyReason)]
      | Parser_flow.Ast.Expression.This => Exp.ident (astHelperLid (Lident "this"))
      | Parser_flow.Ast.Expression.Logical {Logical.operator: operator, left, right} =>
        let operatorReason =
          switch operator {
          | Logical.Or => "||"
          | Logical.And => "&&"
          };
        Exp.apply
          (Exp.ident (astHelperLid (Lident operatorReason)))
          [("", expressionMapper left), ("", expressionMapper right)]
      | Parser_flow.Ast.Expression.Array _
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
      | Parser_flow.Ast.Expression.JSXElement _
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
  let {pexp_desc, _} = statementMapper terminalExpr::None statementWrap;
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
          pvb_pat: Pat.var {loc: default_loc.contents, txt: "topPlaceholderMe"},
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
            pvb_pat: Pat.var {loc: default_loc.contents, txt: "topPlaceholder"},
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
