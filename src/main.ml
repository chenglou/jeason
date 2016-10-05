open Ast_helper
open Asttypes
open Parsetree
open Longident
let defaultStructures =
  [Str.mk
     ((Pstr_value
         (Nonrecursive,
           [{
              pvb_pat =
                (Pat.var { loc = (default_loc.contents); txt = "myVar2" });
              pvb_expr =
                (Exp.construct
                   {
                     loc = (default_loc.contents);
                     txt = ((Lident ("()"))[@explicit_arity ])
                   } None);
              pvb_attributes = [];
              pvb_loc = (default_loc.contents)
            }]))[@explicit_arity ])]
let expressionMapperOld (expression : Parser_flow.Ast.Expression.t) =
  (ignore expression; defaultStructures : Parsetree.structure_item list)
let patternMapper (pattern : Parser_flow.Ast.Pattern.t) =
  (ignore pattern; defaultStructures : Parsetree.structure_item list)
let astHelperStr a = { loc = (default_loc.contents); txt = a }
let astHelperLid (a : Longident.t) =
  { loc = (default_loc.contents); txt = a }
let rec expressionMapper ((_,expression) : Parser_flow.Ast.Expression.t) =
  (let defaultExpression =
     Exp.construct
       {
         loc = (default_loc.contents);
         txt = ((Lident ("()"))[@explicit_arity ])
       } None in
   let open Parser_flow.Ast in
     let open Parser_flow.Ast.Expression in
       match expression with
       | Parser_flow.Ast.Expression.This  -> defaultExpression
       | Parser_flow.Ast.Expression.Array _ -> defaultExpression
       | Parser_flow.Ast.Expression.Object _ -> defaultExpression
       | ((Parser_flow.Ast.Expression.Function
           ({ Function.params = (params,restParam); body; expression;_}))
           [@explicit_arity ]) ->
           (ignore restParam;
            ignore body;
            ignore expression;
            List.fold_left
              (fun expr'  ->
                 fun (_,param)  ->
                   match param with
                   | ((Pattern.Identifier
                       (_,{ Identifier.name = name;_}))[@implicit_arity ]) ->
                       Exp.fun_ "" None
                         (Pat.construct
                            (astHelperLid ((Lident (name))[@explicit_arity ]))
                            None) expr'
                   | _ ->
                       Exp.fun_ "" None (Pat.var (astHelperStr "fixme"))
                         expr')
              (Exp.constant ((Const_int (1))[@explicit_arity ])) params)
       | Parser_flow.Ast.Expression.ArrowFunction _ -> defaultExpression
       | Parser_flow.Ast.Expression.Sequence _ -> defaultExpression
       | Parser_flow.Ast.Expression.Unary _ -> defaultExpression
       | Parser_flow.Ast.Expression.Binary _ -> defaultExpression
       | Parser_flow.Ast.Expression.Assignment _ -> defaultExpression
       | Parser_flow.Ast.Expression.Update _ -> defaultExpression
       | Parser_flow.Ast.Expression.Logical _ -> defaultExpression
       | Parser_flow.Ast.Expression.Conditional _ -> defaultExpression
       | Parser_flow.Ast.Expression.New _ -> defaultExpression
       | ((Parser_flow.Ast.Expression.Call
           ({ Call.callee = (_,callee); arguments }))[@explicit_arity ]) ->
           (match (callee, arguments) with
            | (((Member
               ({
                  Member._object =
                    (_,((Identifier
                     (_,{ Identifier.name = "React";_}))[@implicit_arity ]));
                  property = ((Member.PropertyIdentifier
                    (_,{ Identifier.name = "createClass";_}))[@implicit_arity
                                                               ]);_}))
               [@explicit_arity ]),((Expression
               (_,((Object
                ({ Object.properties = properties }))[@explicit_arity ])))
               [@implicit_arity ])::[]) ->
                let createClassSpec =
                  properties |>
                    (List.map
                       (fun property  ->
                          let open Object in
                            match property with
                            | ((Property
                                (_,{
                                     Property.key = ((Property.Identifier
                                       (_,{ Identifier.name = name;_}))
                                       [@implicit_arity ]);
                                     value = ((_,value) as valueWrap);
                                     kind = Property.Init ;_}))[@implicit_arity
                                                                 ])
                                ->
                                (match value with
                                 | Function _|ArrowFunction _ ->
                                     Cf.method_ (astHelperStr name) Public
                                       ((Cfk_concrete
                                           (Fresh,
                                             (Exp.poly
                                                (expressionMapper valueWrap)
                                                None)))[@explicit_arity ])
                                 | _ ->
                                     Cf.val_
                                       {
                                         loc = (default_loc.contents);
                                         txt = name
                                       } Immutable
                                       ((Cfk_concrete
                                           (Fresh,
                                             (expressionMapper valueWrap)))
                                       [@explicit_arity ]))
                            | _ ->
                                Cf.val_
                                  {
                                    loc = (default_loc.contents);
                                    txt = "notSureWhat"
                                  } Immutable
                                  ((Cfk_concrete
                                      (Fresh,
                                        (Exp.ident
                                           {
                                             loc = (default_loc.contents);
                                             txt =
                                               ((Lident ("thisIs"))[@explicit_arity
                                                                    ])
                                           })))[@explicit_arity ]))) in
                let createClassObj =
                  Exp.object_
                    (Cstr.mk
                       (Pat.mk
                          ((Ppat_var
                              ({ loc = (default_loc.contents); txt = "this" }))
                          [@explicit_arity ])) createClassSpec) in
                Exp.apply
                  (Exp.ident
                     {
                       loc = (default_loc.contents);
                       txt =
                         ((Ldot
                             (((Lident ("ReactRe"))[@explicit_arity ]),
                               "createClass"))[@explicit_arity ])
                     }) [("", createClassObj)]
            | _ -> defaultExpression)
       | Parser_flow.Ast.Expression.Member _ -> defaultExpression
       | Parser_flow.Ast.Expression.Yield _ -> defaultExpression
       | Parser_flow.Ast.Expression.Comprehension _ -> defaultExpression
       | Parser_flow.Ast.Expression.Generator _ -> defaultExpression
       | Parser_flow.Ast.Expression.Let _ -> defaultExpression
       | Parser_flow.Ast.Expression.Identifier _ -> defaultExpression
       | Parser_flow.Ast.Expression.Literal _ -> defaultExpression
       | Parser_flow.Ast.Expression.TemplateLiteral _ -> defaultExpression
       | Parser_flow.Ast.Expression.TaggedTemplate _ -> defaultExpression
       | Parser_flow.Ast.Expression.JSXElement _ -> defaultExpression
       | Parser_flow.Ast.Expression.Class _ -> defaultExpression
       | Parser_flow.Ast.Expression.TypeCast _ -> defaultExpression
       | Parser_flow.Ast.Expression.MetaProperty _ -> defaultExpression : 
  Parsetree.expression)
let statementsMapper statementWrap =
  match statementWrap with
  | (_,statement) ->
      let open Parser_flow.Ast.Statement in
        (match statement with
         | Parser_flow.Ast.Statement.Empty  -> defaultStructures
         | Parser_flow.Ast.Statement.Block _ -> defaultStructures
         | Parser_flow.Ast.Statement.Expression _ -> defaultStructures
         | Parser_flow.Ast.Statement.If _ -> defaultStructures
         | Parser_flow.Ast.Statement.Labeled _ -> defaultStructures
         | Parser_flow.Ast.Statement.Break _ -> defaultStructures
         | Parser_flow.Ast.Statement.Continue _ -> defaultStructures
         | Parser_flow.Ast.Statement.With _ -> defaultStructures
         | Parser_flow.Ast.Statement.TypeAlias _ -> defaultStructures
         | Parser_flow.Ast.Statement.Switch _ -> defaultStructures
         | Parser_flow.Ast.Statement.Return _ -> defaultStructures
         | Parser_flow.Ast.Statement.Throw _ -> defaultStructures
         | Parser_flow.Ast.Statement.Try _ -> defaultStructures
         | Parser_flow.Ast.Statement.While _ -> defaultStructures
         | Parser_flow.Ast.Statement.DoWhile _ -> defaultStructures
         | Parser_flow.Ast.Statement.For _ -> defaultStructures
         | Parser_flow.Ast.Statement.ForIn _ -> defaultStructures
         | Parser_flow.Ast.Statement.ForOf _ -> defaultStructures
         | Parser_flow.Ast.Statement.Let _ -> defaultStructures
         | Parser_flow.Ast.Statement.Debugger  -> defaultStructures
         | Parser_flow.Ast.Statement.FunctionDeclaration _ ->
             defaultStructures
         | ((Parser_flow.Ast.Statement.VariableDeclaration
             ({ VariableDeclaration.kind = kind; declarations }))[@explicit_arity
                                                                   ])
             ->
             (match kind with
              | VariableDeclaration.Let  ->
                  let open Parser_flow.Ast.Statement.VariableDeclaration.Declarator in
                    declarations |>
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
                              | ((Some (expr))[@explicit_arity ]) ->
                                  expressionMapper expr in
                            Str.value Nonrecursive
                              [(match t.id with
                                | (_,((Parser_flow.Ast.Pattern.Identifier
                                   (x))[@explicit_arity ])) ->
                                    let (_,x) = x in
                                    {
                                      pvb_pat =
                                        (Pat.var
                                           {
                                             loc = (default_loc.contents);
                                             txt =
                                               (let open Parser_flow.Ast.Identifier in
                                                  x.name)
                                           });
                                      pvb_expr = initialValue;
                                      pvb_attributes = [];
                                      pvb_loc = (default_loc.contents)
                                    }
                                | _ ->
                                    {
                                      pvb_pat =
                                        (Pat.var
                                           {
                                             loc = (default_loc.contents);
                                             txt = "myVar"
                                           });
                                      pvb_expr = initialValue;
                                      pvb_attributes = [];
                                      pvb_loc = (default_loc.contents)
                                    })]))
              | _ -> defaultStructures)
         | Parser_flow.Ast.Statement.ClassDeclaration _ -> defaultStructures
         | Parser_flow.Ast.Statement.InterfaceDeclaration _ ->
             defaultStructures
         | Parser_flow.Ast.Statement.DeclareVariable _ -> defaultStructures
         | Parser_flow.Ast.Statement.DeclareFunction _ -> defaultStructures
         | Parser_flow.Ast.Statement.DeclareClass _ -> defaultStructures
         | Parser_flow.Ast.Statement.DeclareModule _ -> defaultStructures
         | Parser_flow.Ast.Statement.DeclareModuleExports _ ->
             defaultStructures
         | Parser_flow.Ast.Statement.DeclareExportDeclaration _ ->
             defaultStructures
         | Parser_flow.Ast.Statement.ExportDeclaration _ -> defaultStructures
         | Parser_flow.Ast.Statement.ImportDeclaration _ -> defaultStructures)
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
     (statements |>
        (List.map (fun statementWrap  -> statementsMapper statementWrap)))
       |> List.concat in
   output_value stdout result)