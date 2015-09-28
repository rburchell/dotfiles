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

