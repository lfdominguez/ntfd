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

dbus-send --dest=io.ntfd.services --print-reply \
    /weather org.freedesktop.DBus.Properties.Get \
    string:'io.ntfd.openweathermap.strings' string:'CurrentIcon'

gdbus call --session \
    --dest io.ntfd.services \
    --object-path /weather \
    --method org.freedesktop.DBus.Properties.Get  \
    io.ntfd.openweathermap.strings \
    CurrentIcon

dbus-send --dest=io.ntfd.services --print-reply \
    /weather io.ntfd.openweathermap.strings.CurrentTemperature string:''

gdbus call --session \
    --dest io.ntfd.services \
    --object-path /weather \
    --method io.ntfd.openweathermap.strings.CurrentTemperature \
    celcius

## DBus examples
Current temperature in celcius:
```sh
dbus-send --dest=io.ntfd.services --print-reply=literal \
    /weather io.ntfd.openweathermap.strings.CurrentTemperature string:'celcius'
```

Rendered version of the configured template:
```sh
dbus-send --dest=io.ntfd.services --print-reply=literal \
    /weather io.ntfd.openweathermap.strings.CurrentTemperature string:'celcius'
```

