let run_py_script ~code ~output_dir =
  Lwt_process.(
    exec ~stdout:`Dev_null
      ("", [| "python"; "-c"; [%blob "from_baostock.py"]; code; output_dir |]))
