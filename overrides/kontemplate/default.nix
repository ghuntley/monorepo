{ pkgs, ... }:

with pkgs;

third_party.writeShellScriptBin "kontemplate" ''
  export PATH="${ops.kms_pass}/bin:$PATH"

  if [[ -z $1 ]]; then
    exec ${ops.kontemplate}/bin/kontemplate
  fi

  exec ${ops.kontemplate}/bin/kontemplate $1 ${./../..}/ops/infra/kubernetes/primary-cluster.yaml ''${@:2}
''
