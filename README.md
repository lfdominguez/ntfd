# `ntfd` - Notification daemon
A work in progress notification daemon.

`ntfd` synchronizes with different services and offers synchronous APIs for desktop integration via D-Bus. \
It can be used as a data source for [Polybar](https://github.com/polybar/polybar), [Rofi](https://github.com/davatorium/rofi) or any other similar tool.

It will implement integration with several services such as:
- OperweatherMap (current weather, forecast, alerts ?, should re-implement [`polybar-forecast`](https://github.com/kamek-pf/polybar-forecast))
- Github (unread notifications count, live notifications ?)
- Twitch (followed streams state changes and count, rofi integration)
- Gmail (live notifications, unread messages count, multi account support)
- Facebook (live messages, unread notifications count)
- Reddit (?)

Probably in that order. \
The daemon will be queryable via D-Bus, but stay loosely coupled so that new interfaces can be added.

## Usage
Here are a few example DBus queries you can use from shell scripts, you'll need [`jq`](https://www.archlinux.org/packages/community/x86_64/jq/) to run the examples:

DBus properties:
```sh
# Get the current weather icon
busctl --user -j get-property io.ntfd /weather openweathermap.strings CurrentIcon | jq -r .data

# Rendered version of the configured weather template:
busctl --user -j get-property io.ntfd /weather openweathermap.strings RenderedTemplate | jq -r .data
```

DBus methods:
```sh
# Supported units are "celcius", "kelvin" and "fahrenheit":

# Current temperature in celcius:
busctl --user -j call io.ntfd /weather openweathermap.strings CurrentTemperature s "celcius" | jq -r '.data[0]'

# Forecast temperature in fahrenheit:
busctl --user -j call io.ntfd /weather openweathermap.strings CurrentTemperature s "fahrenheit" | jq -r '.data[0]'
```

To explore the DBus API, I recommend [`d-feet`](https://www.archlinux.org/packages/community/any/d-feet/), a graphical tool to explore DBus interfaces. \
In `d-feet`, go to the Session Bus tab from the top bar, and look for `io.ntfd`.

The [`busctl` documentation](https://www.freedesktop.org/software/systemd/man/busctl.html) might also come in handy, especially for method calls.
