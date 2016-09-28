(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(**
 * Checks if x is a www directory by looking for ".hhconfig".
 *)
let is_www_directory ?(config=".hhconfig") (path : PathFlow.t) : bool =
   let arcconfig = PathFlow.concat path config in
   PathFlow.file_exists arcconfig

let assert_www_directory ?(config=".hhconfig") (path : PathFlow.t) : unit =
   if not (PathFlow.file_exists path && PathFlow.is_directory path)
   then begin
     Printf.eprintf "Error: %s is not a directory\n%!" (PathFlow.to_string path);
     exit 1
   end;
   if not (is_www_directory ~config path)
   then begin
     Printf.fprintf stderr
"Error: could not find a %s file in %s \
 or any of its parent directories. \
 Do you have a %s in your code's root directory?\n"
       config
       (PathFlow.to_string path)
       config;
     flush stderr;
     exit 1
   end
