/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

let defaultStructures = [
  Str.mk (
    Pstr_value
      Nonrecursive
      [
        {
          pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar"},
          pvb_expr: Exp.construct {loc: default_loc.contents, txt: Lident "()"} None,
          pvb_attributes: [],
          pvb_loc: default_loc.contents
        }
      ]
  )
];

let expressionMapperOld (expression: Parser_flow.Ast.Expression.t) :list Parsetree.structure_item => {
  ignore expression;
  /* Exp.construct {loc: default_loc.contents, txt: Lident "()"} None */
  defaultStructures
};

let patternMapper (pattern: Parser_flow.Ast.Pattern.t) :list Parsetree.structure_item => {
  ignore pattern;
  /* Pat.var {loc: default_loc.contents, txt: "myVar"} */
  defaultStructures
};

let expressionMapper ((_, expression): Parser_flow.Ast.Expression.t) :Parsetree.expression => {
  let defaultExpression = Exp.construct {loc: default_loc.contents, txt: Lident "()"} None;
  Parser_flow.Ast.(
    Parser_flow.Ast.Expression.(
      switch expression {
      | Parser_flow.Ast.Expression.This => defaultExpression
      | Parser_flow.Ast.Expression.Array _ => defaultExpression
      | Parser_flow.Ast.Expression.Object _ => defaultExpression
      | Parser_flow.Ast.Expression.Function _ => defaultExpression
      | Parser_flow.Ast.Expression.ArrowFunction _ => defaultExpression
      | Parser_flow.Ast.Expression.Sequence _ => defaultExpression
      | Parser_flow.Ast.Expression.Unary _ => defaultExpression
      | Parser_flow.Ast.Expression.Binary _ => defaultExpression
      | Parser_flow.Ast.Expression.Assignment _ => defaultExpression
      | Parser_flow.Ast.Expression.Update _ => defaultExpression
      | Parser_flow.Ast.Expression.Logical _ => defaultExpression
      | Parser_flow.Ast.Expression.Conditional _ => defaultExpression
      | Parser_flow.Ast.Expression.New _ => defaultExpression
      | Parser_flow.Ast.Expression.Call {Call.callee: (_, callee), arguments} =>
        switch (callee, arguments) {
        | (
            Member {
              Member._object: (_, Identifier _ {Identifier.name: "React", _} [@implicit_arity]),
              property:
                Member.PropertyIdentifier _ {Identifier.name: "createClass", _} [@implicit_arity],
              _
            },
            [Expression _ (Object {Object.properties: properties}) [@implicit_arity]]
          ) =>
          let createClassSpec =
            properties |>
            List.map (
              fun property =>
                Object.(
                  switch property {
                  | SpreadProperty _ =>
                    Cf.val_
                      {loc: default_loc.contents, txt: "thereIsASpreadHere"}
                      Immutable
                      (
                        Cfk_concrete
                          Fresh
                          (
                            Exp.ident {
                              loc: default_loc.contents,
                              txt: Lident "iDontKnowHowToTransformIt"
                            }
                          )
                      )
                  | Property (_, {Property.key: key, value, kind, _method, shorthand}) =>
                    Cf.val_
                      {loc: default_loc.contents, txt: "render"}
                      Immutable
                      (
                        Cfk_concrete
                          Fresh (Exp.ident {loc: default_loc.contents, txt: Lident "kek"})
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
        | _ => defaultExpression
        }
      | Parser_flow.Ast.Expression.Member _ => defaultExpression
      | Parser_flow.Ast.Expression.Yield _ => defaultExpression
      | Parser_flow.Ast.Expression.Comprehension _ => defaultExpression
      | Parser_flow.Ast.Expression.Generator _ => defaultExpression
      | Parser_flow.Ast.Expression.Let _ => defaultExpression
      | Parser_flow.Ast.Expression.Identifier _ => defaultExpression
      | Parser_flow.Ast.Expression.Literal _ => defaultExpression
      | Parser_flow.Ast.Expression.TemplateLiteral _ => defaultExpression
      | Parser_flow.Ast.Expression.TaggedTemplate _ => defaultExpression
      | Parser_flow.Ast.Expression.JSXElement _ => defaultExpression
      | Parser_flow.Ast.Expression.Class _ => defaultExpression
      | Parser_flow.Ast.Expression.TypeCast _ => defaultExpression
      | Parser_flow.Ast.Expression.MetaProperty _ => defaultExpression
      }
    )
  )
};

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
        switch kind {
        | VariableDeclaration.Let =>
          Parser_flow.Ast.Statement.VariableDeclaration.Declarator.(
            declarations |>
            List.map (
              fun (_, t) => {
                let initialValue: Parsetree.expression =
                  switch t.init {
                  /* TODO: turn into option? */
                  | None => Exp.construct {loc: default_loc.contents, txt: Lident "()"} None
                  | Some expr => expressionMapper expr
                  };
                Str.value
                  Nonrecursive
                  [
                    {
                      pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar"},
                      pvb_expr: initialValue,
                      pvb_attributes: [],
                      pvb_loc: default_loc.contents
                    }
                  ]
              }
            )
          )
        | _ => defaultStructures
        }
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
