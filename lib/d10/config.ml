type clk = [ `Clock of float ] Eio.Resource.t

type t = {
  sys : Sysops.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  clock : clk;
  root : Eio.Fs.dir_ty Eio.Path.t;
  os_key : string;
}
