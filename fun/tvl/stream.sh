#!/usr/bin/env bash

env LD_LIBRARY_PATH=/run/opengl-driver/lib/ ffmpeg \
     -vsync 0 \
     -hwaccel cuvid \
     -f x11grab \
     -video_size 1920x1080 \
     -framerate 60 \
     -i :0.0+0,0 \
     -vf "scale_npp=1280:720" \
     -c:v h264_nvenc \
     -preset fast \
     -an \
     -f flv rtmp://tazj.in:1935/tvl
