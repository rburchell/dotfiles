function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
hs.dockicon.hide()
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Hammerspoon config loaded")

-- animations are super jerky and annoying.
hs.window.animationDuration = 0

hs.hotkey.bind({"cmd", "alt"}, "Left", function()
  local win = hs.window.frontmostWindow()
  win:moveOneScreenWest()
end)

hs.hotkey.bind({"cmd", "alt"}, "Right", function()
  local win = hs.window.frontmostWindow()
  win:moveOneScreenEast()
end)

hs.hotkey.bind({"cmd"}, "Right", function()
  local win = hs.window.frontmostWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd"}, "Left", function()
  local win = hs.window.frontmostWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.x + (max.w / 2)
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd"}, "Up", function()
  local win = hs.window.frontmostWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.x + max.w
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd"}, "Down", function()
  hs.window.frontmostWindow():minimize()
end)

function audioWatchCallback(arg)
	-- If the default audio sink changes, then mute the volume.
	-- This prevents annoying situations like losing BT or microphone jack
	-- making me the center of attention.
	if arg == "dOut" then
		hs.audiodevice.defaultOutputDevice():setVolume(0)
	end
end

hs.audiodevice.watcher.setCallback(audioWatchCallback)
hs.audiodevice.watcher.start()

function usbWatchCallback(args)
    local eventType = args["eventType"]
    local productName = args["productName"]
    local vendorName = args["vendorName"]
    local vendorId = args["vendorId"]
    local productId = args["productId"]

    if eventType == "added" then
        local str = string.format("USB device %s from %s plugged in", productName, vendorName)
        hs.notify.show("USB inserted", "", str)
    elseif eventType == "removed" then
        local str = string.format("USB device %s from %s unplugged", productName, vendorName)
        hs.notify.show("USB removed", "", str)
    end
end

local usbWatcher = hs.usb.watcher.new(usbWatchCallback)
usbWatcher:start()
