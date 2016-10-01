/* open Ast_mapper */
open Ast_helper;

open Asttypes;

open Parsetree;

/* open Longident */
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
  /* let (ast, _) = Parser_flow.program_file ~fail:false ~parse_options content (Some file) in */
  let (ast, _) =
    Parser_flow.program_file fail::false parse_options::parse_options "let asd = 1" None;
  let (_, b, _) = ast;
  switch b {
  | [] => print_endline "nothing"
  | [a, ..._] =>
    switch a {
    | (_, b) =>
      Parser_flow.Ast.Statement.(
        switch b {
        | Parser_flow.Ast.Statement.Empty => print_endline "Empty"
        | Parser_flow.Ast.Statement.Block _ => print_endline "Block"
        | Parser_flow.Ast.Statement.Expression _ => print_endline "Expression"
        | Parser_flow.Ast.Statement.If _ => print_endline "If"
        | Parser_flow.Ast.Statement.Labeled _ => print_endline "Labeled"
        | Parser_flow.Ast.Statement.Break _ => print_endline "Break"
        | Parser_flow.Ast.Statement.Continue _ => print_endline "Continue"
        | Parser_flow.Ast.Statement.With _ => print_endline "With"
        | Parser_flow.Ast.Statement.TypeAlias _ => print_endline "TypeAlias"
        | Parser_flow.Ast.Statement.Switch _ => print_endline "Switch"
        | Parser_flow.Ast.Statement.Return _ => print_endline "Return"
        | Parser_flow.Ast.Statement.Throw _ => print_endline "Throw"
        | Parser_flow.Ast.Statement.Try _ => print_endline "Try"
        | Parser_flow.Ast.Statement.While _ => print_endline "While"
        | Parser_flow.Ast.Statement.DoWhile _ => print_endline "DoWhile"
        | Parser_flow.Ast.Statement.For _ => print_endline "For"
        | Parser_flow.Ast.Statement.ForIn _ => print_endline "ForIn"
        | Parser_flow.Ast.Statement.ForOf _ => print_endline "ForOf"
        | Parser_flow.Ast.Statement.Let _ => print_endline "Let"
        | Parser_flow.Ast.Statement.Debugger => print_endline "Debugger"
        | Parser_flow.Ast.Statement.FunctionDeclaration _ => print_endline "FunctionDeclaration"
        | Parser_flow.Ast.Statement.VariableDeclaration {
            VariableDeclaration.kind: kind,
            declarations
          } =>
          switch kind {
          | VariableDeclaration.Let =>
            ignore declarations;
            output_string stdout Config.ast_impl_magic_number;
            output_value stdout "fileNameThisIsIgnored.ml";
            output_value
              stdout
              [
                Str.value
                  Nonrecursive
                  [
                    {
                      pvb_pat: Pat.var {loc: default_loc.contents, txt: "myVar"},
                      /* pvb_pat: Pat.var {loc: default_loc.contents, txt: ""}, */
                      pvb_expr: Exp.constant (Const_string "asd" None [@implicit_arity]),
                      pvb_attributes: [],
                      pvb_loc: default_loc.contents
                    }
                  ]
              ]
          | _ => print_endline "else"
          }
        | Parser_flow.Ast.Statement.ClassDeclaration _ => print_endline "ClassDeclaration"
        | Parser_flow.Ast.Statement.InterfaceDeclaration _ => print_endline "InterfaceDeclaration"
        | Parser_flow.Ast.Statement.DeclareVariable _ => print_endline "DeclareVariable"
        | Parser_flow.Ast.Statement.DeclareFunction _ => print_endline "DeclareFunction"
        | Parser_flow.Ast.Statement.DeclareClass _ => print_endline "DeclareClass"
        | Parser_flow.Ast.Statement.DeclareModule _ => print_endline "DeclareModule"
        | Parser_flow.Ast.Statement.DeclareModuleExports _ => print_endline "DeclareModuleExports"
        | Parser_flow.Ast.Statement.DeclareExportDeclaration _ =>
          print_endline "DeclareExportDeclaration"
        | Parser_flow.Ast.Statement.ExportDeclaration _ => print_endline "ExportDeclaration"
        | Parser_flow.Ast.Statement.ImportDeclaration _ => print_endline "ImportDeclaration"
        }
      )
    }
  }
};
