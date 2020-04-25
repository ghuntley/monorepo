# iDual light control

This folder contains some tooling for controlling iDual LED lights
(which use infrared controls) using a "Broadlink RM Pro" infrared
controller.

The supported colour codes of the iDual remote are stored in
`codes.txt`.

The point of this is to make it possible for me to automate my lights
in the morning, so that I can actually get out of bed.

## Capturing codes

Capturing codes is relatively easy, assuming that the broadlink device
is set up:

```python
import broadlink
import base64

devices = broadlink.discover(timeout=5)
devices[0].auth()
```

For each code, the procedure is as follows:

```python
devices[0].find_rf_packet()
# wait until this returns True

devices[0].check_data()
# this will return the code
```
