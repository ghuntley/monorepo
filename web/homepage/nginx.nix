# This file creates an nginx server that serves the blog on port 8080.
#
# It's not intended to be the user-facing nginx.
{
  # third_party attributes supplied by callPackage
  writeText, writeShellScriptBin, nginx, lib,

  # website content
  blog, website
}:

let
  inherit (builtins) hasAttr filter map;
  inherit (pkgs.third_party) ;

  oldRedirects = lib.concatStringsSep "\n" (map (post: ''
    location ~* ^(/en)?/${post.oldKey} {
      # TODO(tazjin): 301 once this works
      return 302 https://tazj.in/blog/${post.key};
    }
  '') (filter (hasAttr "oldKey") blog.posts));

  config = writeText "homepage-nginx.conf" ''
    daemon off;
    worker_processes 1;
    error_log stderr;
    pid /tmp/nginx-homepage.pid;

    events {
      worker_connections  1024;
    }

    http {
      include ${nginx}/conf/mime.types;
      fastcgi_temp_path /tmp/nginx-homepage;
      uwsgi_temp_path /tmp/nginx-homepage;
      scgi_temp_path /tmp/nginx-homepage;
      client_body_temp_path /tmp/nginx-homepage;
      proxy_temp_path /tmp/nginx-homepage;
      sendfile on;

      # Logging is handled by the primary nginx server
      access_log off;

      server {
        listen 8080 default_server;
        server_name tazj.in;
        root ${website};

        ${oldRedirects}

        location /blog {
          alias ${blog.rendered};

          if ($request_uri ~ ^/(.*)\.html$) {
            return 302 /$1;
          }

          try_files $uri $uri.html $uri/ =404;
        }
      }

      server {
        listen 8080;
        server_name www.tazj.in;
        return 301 https://tazj.in$request_uri;
      }
    }
  '';
in writeShellScriptBin "homepage" ''
  if [[ -v CONTAINER_SETUP ]]; then
    cd /run
    echo 'nogroup:x:30000:nobody' >> /etc/group
    echo 'nobody:x:30000:30000:nobody:/tmp:/bin/bash' >> /etc/passwd
  fi

  mkdir -p /tmp/nginx-homepage
  exec ${nginx}/bin/nginx -c ${config}
''
