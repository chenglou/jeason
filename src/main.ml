open Ast_helper
open Asttypes
open Parsetree
open Longident
let defaultStructure =
  Str.mk
    ((Pstr_value
        (Nonrecursive,
          [{
             pvb_pat =
               (Pat.var { loc = (default_loc.contents); txt = "myVar" });
             pvb_expr =
               (Exp.construct
                  {
                    loc = (default_loc.contents);
                    txt = ((Lident ("()"))[@explicit_arity ])
                  } None);
             pvb_attributes = [];
             pvb_loc = (default_loc.contents)
           }]))[@explicit_arity ])
let expressionMapper (expression : Parser_flow.Ast.Expression.t) =
  (ignore expression; defaultStructure : Parsetree.structure_item)
let patternMapper (pattern : Parser_flow.Ast.Pattern.t) =
  (ignore pattern; defaultStructure : Parsetree.structure_item)
let statementsMapper statementWrap =
  match statementWrap with
  | (_,statement) ->
      let open Parser_flow.Ast.Statement in
        (match statement with
         | Parser_flow.Ast.Statement.Empty  -> defaultStructure
         | Parser_flow.Ast.Statement.Block _ -> defaultStructure
         | Parser_flow.Ast.Statement.Expression _ -> defaultStructure
         | Parser_flow.Ast.Statement.If _ -> defaultStructure
         | Parser_flow.Ast.Statement.Labeled _ -> defaultStructure
         | Parser_flow.Ast.Statement.Break _ -> defaultStructure
         | Parser_flow.Ast.Statement.Continue _ -> defaultStructure
         | Parser_flow.Ast.Statement.With _ -> defaultStructure
         | Parser_flow.Ast.Statement.TypeAlias _ -> defaultStructure
         | Parser_flow.Ast.Statement.Switch _ -> defaultStructure
         | Parser_flow.Ast.Statement.Return _ -> defaultStructure
         | Parser_flow.Ast.Statement.Throw _ -> defaultStructure
         | Parser_flow.Ast.Statement.Try _ -> defaultStructure
         | Parser_flow.Ast.Statement.While _ -> defaultStructure
         | Parser_flow.Ast.Statement.DoWhile _ -> defaultStructure
         | Parser_flow.Ast.Statement.For _ -> defaultStructure
         | Parser_flow.Ast.Statement.ForIn _ -> defaultStructure
         | Parser_flow.Ast.Statement.ForOf _ -> defaultStructure
         | Parser_flow.Ast.Statement.Let _ -> defaultStructure
         | Parser_flow.Ast.Statement.Debugger  -> defaultStructure
         | Parser_flow.Ast.Statement.FunctionDeclaration _ ->
             defaultStructure
         | ((Parser_flow.Ast.Statement.VariableDeclaration
             ({ VariableDeclaration.kind = kind; declarations }))[@explicit_arity
                                                                   ])
             ->
             (match kind with
              | VariableDeclaration.Let  ->
                  let open Parser_flow.Ast.Statement.VariableDeclaration.Declarator in
                    (declarations |>
                       (List.map
                          (fun (_,t)  ->
                             let initialValue: Parsetree.expression =
                               match t.init with
                               | None  ->
                                   Exp.construct
                                     {
                                       loc = (default_loc.contents);
                                       txt =
                                         ((Lident ("()"))[@explicit_arity ])
                                     } None
                               | Some _ ->
                                   Exp.construct
                                     {
                                       loc = (default_loc.contents);
                                       txt =
                                         ((Lident ("()"))[@explicit_arity ])
                                     } None in
                             ignore initialValue;
                             Str.value Nonrecursive
                               [{
                                  pvb_pat =
                                    (Pat.var
                                       {
                                         loc = (default_loc.contents);
                                         txt = "myVar"
                                       });
                                  pvb_expr =
                                    (Exp.construct
                                       {
                                         loc = (default_loc.contents);
                                         txt =
                                           ((Lident ("()"))[@explicit_arity ])
                                       } None);
                                  pvb_attributes = [];
                                  pvb_loc = (default_loc.contents)
                                }])))
                      |> List.hd
              | _ -> defaultStructure)
         | Parser_flow.Ast.Statement.ClassDeclaration _ -> defaultStructure
         | Parser_flow.Ast.Statement.InterfaceDeclaration _ ->
             defaultStructure
         | Parser_flow.Ast.Statement.DeclareVariable _ -> defaultStructure
         | Parser_flow.Ast.Statement.DeclareFunction _ -> defaultStructure
         | Parser_flow.Ast.Statement.DeclareClass _ -> defaultStructure
         | Parser_flow.Ast.Statement.DeclareModule _ -> defaultStructure
         | Parser_flow.Ast.Statement.DeclareModuleExports _ ->
             defaultStructure
         | Parser_flow.Ast.Statement.DeclareExportDeclaration _ ->
             defaultStructure
         | Parser_flow.Ast.Statement.ExportDeclaration _ -> defaultStructure
         | Parser_flow.Ast.Statement.ImportDeclaration _ -> defaultStructure)
let () =
  let parse_options =
    Some
      (let open Parser_env in
         {
           esproposal_class_instance_fields = true;
           esproposal_class_static_fields = true;
           esproposal_decorators = true;
           esproposal_export_star_as = true;
           types = true;
           use_strict = false
         }) in
  let file = "test.js" in
  let content = Sys_utils.cat file in
  let (ast,_) =
    Parser_flow.program_file ~fail:false ~parse_options content
      (Some ((Loc.SourceFile (file))[@explicit_arity ])) in
  let (_,statements,_) = ast in
  output_string stdout Config.ast_impl_magic_number;
  output_value stdout file;
  (let result: Parsetree.structure =
     statements |>
       (List.map (fun statementWrap  -> statementsMapper statementWrap)) in
   output_value stdout result)