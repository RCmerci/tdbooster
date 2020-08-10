let run_py_script ~code ~output_dir =
  Lwt_process.(exec ~stdout:`Dev_null ("", [|"python";"-c";[%blob "from_baostock_ttm.py"]; code; output_dir|]))  
