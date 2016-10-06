/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

let expUnit = Exp.construct {loc: default_loc.contents, txt: Lident "()"} None;

let placeholder = Exp.constant (Const_string "placeholder" None);

let defaultStructures = [
  Str.mk (
    Pstr_value
      Nonrecursive
      [
        {
          pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar2"},
          pvb_expr: expUnit,
          pvb_attributes: [],
          pvb_loc: default_loc.contents
        }
      ]
  )
];

let astHelperStr a => {loc: default_loc.contents, txt: a};

let astHelperLid (a: Longident.t) => {loc: default_loc.contents, txt: a};

let parseTreeValueBinding pat::pat expr::expr => {
  pvb_pat: pat,
  pvb_expr: expr,
  pvb_attributes: [],
  pvb_loc: default_loc.contents
};

let rec expressionMapper ((_, expression): Parser_flow.Ast.Expression.t) :Parsetree.expression =>
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      switch expression {
      | Parser_flow.Ast.Expression.This => placeholder
      | Parser_flow.Ast.Expression.Array _ => placeholder
      | Parser_flow.Ast.Expression.Object _ => placeholder
      | Parser_flow.Ast.Expression.ArrowFunction {Function.params: (params, restParam), body, _}
      | Parser_flow.Ast.Expression.Function {Function.params: (params, restParam), body, _} =>
        ignore restParam;
        ignore body;
        let bodyReason =
          switch body {
          | Function.BodyBlock (_, {Statement.Block.body: body}) =>
            switch body {
            | [] => Exp.constant (Const_string "noBodyItemOk" None)
            | bodyNotEmpty =>
              let bodyNotEmptyFlipped = List.rev bodyNotEmpty;
              let (_, lastItem) = List.hd bodyNotEmptyFlipped;
              let lastItemReason: Parsetree.expression =
                switch lastItem {
                | Statement.VariableDeclaration {
                    Statement.VariableDeclaration.declarations: declarations,
                    kind
                  } =>
                  /* this is the part that transforms non-top-level var declarations list (in a single var
                     declaration) from js to like, let with a tuple or something in reason */
                  /* TODO: this lol */
                  ignore kind;
                  let (_, {Statement.VariableDeclaration.Declarator.id: (_, id), init}) = List.hd declarations;
                  let expr =
                    switch init {
                    | None => expUnit
                    | Some e => expressionMapper e
                    };
                  switch id {
                  | Pattern.Identifier (_, {Identifier.name: name, _}) =>
                    Exp.let_
                      Nonrecursive
                      [parseTreeValueBinding pat::(Pat.var (astHelperStr name)) expr::expr]
                      expUnit
                  | _ => Exp.constant (Const_string "ehOk" None)
                  }
                | _ => Exp.constant (Const_string "noBodyItemOk2" None)
                };
              List.fold_left
                (
                  fun accumExp (_, statement) =>
                    switch statement {
                    | Statement.VariableDeclaration {
                        Statement.VariableDeclaration.declarations: declarations,
                        kind
                      } =>
                      /* TODO: multiple declarations */
                      ignore kind;
                      let (_, {Statement.VariableDeclaration.Declarator.id: (_, id), init}) = List.hd declarations;
                      let name =
                        switch id {
                        | Pattern.Identifier (_, {Identifier.name: name, _}) => name
                        | _ => "okCantDes"
                        };
                      let expr =
                        switch init {
                        | None => expUnit
                        | Some e => expressionMapper e
                        };
                      Exp.let_
                        Nonrecursive
                        [parseTreeValueBinding pat::(Pat.var (astHelperStr name)) expr::expr]
                        accumExp
                    | Statement.Expression {
                        Statement.Expression.expression: (_, expression) as expressionWrap
                      } =>
                      Exp.sequence (expressionMapper expressionWrap) accumExp
                    | _ => Exp.constant (Const_string "bail" None)
                    }
                )
                lastItemReason
                (List.tl bodyNotEmptyFlipped)
            }
          | Function.BodyExpression _ => Exp.constant (Const_string "noBodyItemOk3" None)
          };
        List.fold_left
          (
            fun expr' (_, param) =>
              switch param {
              | Pattern.Identifier (_, {Identifier.name: name, _}) =>
                Exp.fun_ "" None (Pat.construct (astHelperLid (Lident name)) None) expr'
              | _ => Exp.fun_ "" None (Pat.var (astHelperStr "fixme")) expr'
              }
          )
          bodyReason
          params
      | Parser_flow.Ast.Expression.Sequence _ => placeholder
      | Parser_flow.Ast.Expression.Unary _ => placeholder
      | Parser_flow.Ast.Expression.Binary _ => placeholder
      | Parser_flow.Ast.Expression.Assignment _ => placeholder
      | Parser_flow.Ast.Expression.Update _ => placeholder
      | Parser_flow.Ast.Expression.Logical _ => placeholder
      | Parser_flow.Ast.Expression.Conditional _ => placeholder
      | Parser_flow.Ast.Expression.New _ => placeholder
      | Parser_flow.Ast.Expression.Call {Call.callee: (_, callee), arguments} =>
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
          let createClassObj = Exp.object_ (
            Cstr.mk (Pat.mk (Ppat_var {loc: default_loc.contents, txt: "this"})) createClassSpec
          );
          Exp.apply
            (Exp.ident {loc: default_loc.contents, txt: Ldot (Lident "ReactRe") "createClass"})
            [("", createClassObj)]
        | _ => placeholder
        }
      | Parser_flow.Ast.Expression.Member _ => placeholder
      | Parser_flow.Ast.Expression.Yield _ => placeholder
      | Parser_flow.Ast.Expression.Comprehension _ => placeholder
      | Parser_flow.Ast.Expression.Generator _ => placeholder
      | Parser_flow.Ast.Expression.Let _ => placeholder
      | Parser_flow.Ast.Expression.Identifier _ => placeholder
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
        | Literal.RegExp _ => placeholder
        }
      | Parser_flow.Ast.Expression.TemplateLiteral _ => placeholder
      | Parser_flow.Ast.Expression.TaggedTemplate _ => placeholder
      | Parser_flow.Ast.Expression.JSXElement _ => placeholder
      | Parser_flow.Ast.Expression.Class _ => placeholder
      | Parser_flow.Ast.Expression.TypeCast _ => placeholder
      | Parser_flow.Ast.Expression.MetaProperty _ => placeholder
      }
    )
  );

let statementsMapper statementWrap =>
  switch statementWrap {
  | (_, statement) =>
    Parser_flow.Ast.Statement.(
      switch statement {
      | Parser_flow.Ast.Statement.Empty => defaultStructures
      | Parser_flow.Ast.Statement.Block _ => defaultStructures
      | Parser_flow.Ast.Statement.Expression _ => defaultStructures
      | Parser_flow.Ast.Statement.If _ => defaultStructures
      | Parser_flow.Ast.Statement.Labeled _ => defaultStructures
      | Parser_flow.Ast.Statement.Break _ => defaultStructures
      | Parser_flow.Ast.Statement.Continue _ => defaultStructures
      | Parser_flow.Ast.Statement.With _ => defaultStructures
      | Parser_flow.Ast.Statement.TypeAlias _ => defaultStructures
      | Parser_flow.Ast.Statement.Switch _ => defaultStructures
      | Parser_flow.Ast.Statement.Return _ => defaultStructures
      | Parser_flow.Ast.Statement.Throw _ => defaultStructures
      | Parser_flow.Ast.Statement.Try _ => defaultStructures
      | Parser_flow.Ast.Statement.While _ => defaultStructures
      | Parser_flow.Ast.Statement.DoWhile _ => defaultStructures
      | Parser_flow.Ast.Statement.For _ => defaultStructures
      | Parser_flow.Ast.Statement.ForIn _ => defaultStructures
      | Parser_flow.Ast.Statement.ForOf _ => defaultStructures
      | Parser_flow.Ast.Statement.Let _ => defaultStructures
      | Parser_flow.Ast.Statement.Debugger => defaultStructures
      | Parser_flow.Ast.Statement.FunctionDeclaration _ => defaultStructures
      | Parser_flow.Ast.Statement.VariableDeclaration {
          VariableDeclaration.kind: kind,
          declarations
        } =>
        Parser_flow.Ast.Statement.VariableDeclaration.Declarator.(
          declarations |>
          List.map (
            fun (_, t) => {
              let initialValue: Parsetree.expression =
                switch t.init {
                /* TODO: turn into option? */
                | None => placeholder
                | Some expr => expressionMapper expr
                };
              Str.value
                Nonrecursive
                [
                  switch t.id {
                  | (_, Parser_flow.Ast.Pattern.Identifier x) =>
                    let (_, x) = x;
                    {
                      pvb_pat: Pat.var {
                        loc: default_loc.contents,
                        txt: Parser_flow.Ast.Identifier.(x.name)
                      },
                      pvb_expr: initialValue,
                      pvb_attributes: [],
                      pvb_loc: default_loc.contents
                    }
                  | _ => {
                      pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar"},
                      pvb_expr: initialValue,
                      pvb_attributes: [],
                      pvb_loc: default_loc.contents
                    }
                  }
                ]
            }
          )
        )
      | Parser_flow.Ast.Statement.ClassDeclaration _ => defaultStructures
      | Parser_flow.Ast.Statement.InterfaceDeclaration _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareVariable _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareFunction _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareClass _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareModule _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareModuleExports _ => defaultStructures
      | Parser_flow.Ast.Statement.DeclareExportDeclaration _ => defaultStructures
      | Parser_flow.Ast.Statement.ExportDeclaration _ => defaultStructures
      | Parser_flow.Ast.Statement.ImportDeclaration _ => defaultStructures
      }
    )
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
    statements |> List.map (fun statementWrap => statementsMapper statementWrap) |> List.concat;
  output_value stdout result
};
