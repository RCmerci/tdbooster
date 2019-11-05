open Core
let run_py_script ~code ~output_dir =
  Shexp_process.(eval ~context:(Context.create ~stdout:(Unix.openfile ~mode:[Unix.O_WRONLY] "/dev/null") ())
                   (call ["python";"-c";[%blob "from_baostock.py"]; code; output_dir]));
