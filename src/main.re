/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

open Longident;

let defaultStructure = Str.mk (
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
);

let expressionMapper (expression: Parser_flow.Ast.Expression.t) :Parsetree.structure_item => {
  ignore expression;
  /* Exp.construct {loc: default_loc.contents, txt: Lident "()"} None */
  defaultStructure
};

let patternMapper (pattern: Parser_flow.Ast.Pattern.t) :Parsetree.structure_item => {
  ignore pattern;
  /* Pat.var {loc: default_loc.contents, txt: "myVar"} */
  defaultStructure
};

let statementsMapper statementWrap =>
  switch statementWrap {
  | (_, statement) =>
    Parser_flow.Ast.Statement.(
      switch statement {
      | Parser_flow.Ast.Statement.Empty => defaultStructure
      | Parser_flow.Ast.Statement.Block _ => defaultStructure
      | Parser_flow.Ast.Statement.Expression _ => defaultStructure
      | Parser_flow.Ast.Statement.If _ => defaultStructure
      | Parser_flow.Ast.Statement.Labeled _ => defaultStructure
      | Parser_flow.Ast.Statement.Break _ => defaultStructure
      | Parser_flow.Ast.Statement.Continue _ => defaultStructure
      | Parser_flow.Ast.Statement.With _ => defaultStructure
      | Parser_flow.Ast.Statement.TypeAlias _ => defaultStructure
      | Parser_flow.Ast.Statement.Switch _ => defaultStructure
      | Parser_flow.Ast.Statement.Return _ => defaultStructure
      | Parser_flow.Ast.Statement.Throw _ => defaultStructure
      | Parser_flow.Ast.Statement.Try _ => defaultStructure
      | Parser_flow.Ast.Statement.While _ => defaultStructure
      | Parser_flow.Ast.Statement.DoWhile _ => defaultStructure
      | Parser_flow.Ast.Statement.For _ => defaultStructure
      | Parser_flow.Ast.Statement.ForIn _ => defaultStructure
      | Parser_flow.Ast.Statement.ForOf _ => defaultStructure
      | Parser_flow.Ast.Statement.Let _ => defaultStructure
      | Parser_flow.Ast.Statement.Debugger => defaultStructure
      | Parser_flow.Ast.Statement.FunctionDeclaration _ => defaultStructure
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
                  | Some _ => Exp.construct {loc: default_loc.contents, txt: Lident "()"} None
                  };
                ignore initialValue;
                Str.value
                  Nonrecursive
                  [
                    {
                      pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar"},
                      pvb_expr: Exp.construct {loc: default_loc.contents, txt: Lident "()"} None,
                      pvb_attributes: [],
                      pvb_loc: default_loc.contents
                    }
                  ]
              }
            ) |> List.hd
          )
        | _ => defaultStructure
        }
      | Parser_flow.Ast.Statement.ClassDeclaration _ => defaultStructure
      | Parser_flow.Ast.Statement.InterfaceDeclaration _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareVariable _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareFunction _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareClass _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareModule _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareModuleExports _ => defaultStructure
      | Parser_flow.Ast.Statement.DeclareExportDeclaration _ => defaultStructure
      | Parser_flow.Ast.Statement.ExportDeclaration _ => defaultStructure
      | Parser_flow.Ast.Statement.ImportDeclaration _ => defaultStructure
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
    statements |> List.map (fun statementWrap => statementsMapper statementWrap);
  output_value stdout result
};
