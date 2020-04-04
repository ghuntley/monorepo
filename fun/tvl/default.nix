{ depot, ... }:

depot.third_party.writeShellScriptBin "start-tvl-stream" ''
  env LD_LIBRARY_PATH=/run/opengl-driver/lib/ ${depot.third_party.ffmpeg}/bin/ffmpeg \
       -vsync 0 \
       -hwaccel cuvid \
       -init_hw_device cuda=0 -filter_hw_device 0 \
       -f x11grab \
       -video_size 1920x1080 \
       -framerate 30 \
       -i :0.0+0,0 \
       -filter:v "format=nv12,hwupload,scale_npp=w=1280:h=720:interp_algo=lanczos" \
       -c:v h264_nvenc \
       -preset:v llhq \
       -rc:v cbr_ld_hq \
       -an \
       -f flv rtmp://tazj.in:1935/tvl
''
