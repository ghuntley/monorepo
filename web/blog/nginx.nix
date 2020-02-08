# This file creates an nginx server that serves the blog on port 8080.
#
# It's not intended to be the user-facing nginx.
{ pkgs, lib, posts, renderedBlog, ... }:

let
  inherit (builtins) hasAttr filter map;
  inherit (pkgs.third_party) writeText writeShellScriptBin nginx;

  oldRedirects = lib.concatStringsSep "\n" (map (post: ''
    location ~* ^(en)?/${post.oldKey} {
      # TODO(tazjin): 301 once this works
      return 302 /${post.key};
    }
  '') (filter (hasAttr "oldKey") posts));

  config = writeText "blog-nginx.conf" ''
    daemon off;
    worker_processes 1;
    error_log stderr;
    pid /tmp/nginx-tazblog.pid;

    events {
      worker_connections  1024;
    }

    http {
      include ${nginx}/conf/mime.types;
      fastcgi_temp_path /tmp/nginx-tazblog;
      uwsgi_temp_path /tmp/nginx-tazblog;
      scgi_temp_path /tmp/nginx-tazblog;
      client_body_temp_path /tmp/nginx-tazblog;
      proxy_temp_path /tmp/nginx-tazblog;
      sendfile on;

      # Logging is handled by the primary nginx server
      access_log off;

      server {
        listen 8080 default_server;
        root ${renderedBlog};

        location /static {
          alias ${./static}/;
        }

        ${oldRedirects}

        location / {
          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }
      }
    }
  '';
in writeShellScriptBin "tazblog" ''
  if [[ -v CONTAINER_SETUP ]]; then
    cd /run
    echo 'nogroup:x:30000:nobody' >> /etc/group
    echo 'nobody:x:30000:30000:nobody:/tmp:/bin/bash' >> /etc/passwd
  fi

  mkdir -p /tmp/nginx-tazblog
  exec ${pkgs.third_party.nginx}/bin/nginx -c ${config}
''