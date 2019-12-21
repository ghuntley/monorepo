# simple oneshot

Run `demo-notify@hello.service`

# simple timer

Run `demo-timer.timer`, show both

# enabling

Enable `demo-timer.timer`, go to symlink folder, disable

# OnError

Show & run `demo-error.service`

# cgroups demo

Start `demo-stress.service` without, show in htop, stop
Show slice unit, start slice unit
Add Slice=demo-limits.slice
daemon-reload
Start stress again

# Proper service

Look at nginx unit
