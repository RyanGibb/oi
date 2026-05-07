type pkg_event =
  | Started of { pkg : string; phase : string }
  | Cached of { pkg : string }
  | Built of { pkg : string }
  | Build_failed of { pkg : string; log : string }
  | Dep_failed of { pkg : string; upstream_log : string }
  | Install_failed of { pkg : string; log : string }

type reporter = { pkg_event : pkg_event -> unit }
