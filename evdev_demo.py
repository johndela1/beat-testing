from evdev import InputDevice, categorize, ecodes

dev = InputDevice('/dev/input/event13')
prev = 0.0
for e in dev.read_loop():
    if e.type == ecodes.EV_KEY and e.value == 1:
        now = e.timestamp()
        print(now - prev)
        prev = now
