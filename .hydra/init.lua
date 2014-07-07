hydra.alert("Hydra config loaded")
pathwatcher.new(os.getenv("HOME") .. "/.hydra/", hydra.reload):start()

-- open a repl
--   the repl is a Lua prompt; type "print('hello world')"
--   when you're in the repl, type "help" to get started
--   almost all readline functionality works in the repl
hotkey.bind({"cmd", "ctrl", "alt"}, "R", repl.open)

-- save the time when updates are checked
function checkforupdates()
  updates.check()
  settings.set('lastcheckedupdates', os.time())
end

-- show a helpful menu
menu.show(function()
    local updatetitles = {[true] = "Install Update", [false] = "Check for Update..."}
    local updatefns = {[true] = updates.install, [false] = checkforupdates}
    local hasupdate = (updates.newversion ~= nil)

    return {
      {title = "Reload Config", fn = hydra.reload},
      {title = "-"},
      {title = "About", fn = hydra.showabout},
      {title = updatetitles[hasupdate], fn = updatefns[hasupdate]},
      {title = "Quit Hydra", fn = os.exit},
    }
end)

-- show available updates
local function showupdate()
  os.execute('open https://github.com/sdegutis/Hydra/releases')
end

-- what to do when an udpate is checked
function updates.available(available)
  if available then
    notify.show("Hydra update available", "", "Click here to see the changelog and maybe even install it", "showupdate")
  else
    hydra.alert("No update available.")
  end
end

-- Uncomment this if you want Hydra to make sure it launches at login
autolaunch.set(true)

-- check for updates every week
timer.new(timer.weeks(1), checkforupdates):start()
notify.register("showupdate", showupdate)

-- if this is your first time running Hydra, you're launching it more than a week later, check now
local lastcheckedupdates = settings.get('lastcheckedupdates')
if lastcheckedupdates == nil or lastcheckedupdates <= os.time() - timer.days(7) then
  checkforupdates()
end

local cmd = { "cmd" }
local cac = { "cmd", "alt", "ctrl" }
local ca  = { "alt", "ctrl" }


local winfuncs = {}

function winfuncs.maximize_window()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w,
        h = screenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 2, screenrect.y + screenrect.h / 2))
end

function winfuncs.top_half()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 2, screenrect.y + screenrect.h / 4))
end

function winfuncs.bottom_half()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y + screenrect.h / 2,
        w = screenrect.w,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 2, screenrect.y + screenrect.h / 4 * 3))
end

function winfuncs.left_half()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w / 2,
        h = screenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4, screenrect.y + screenrect.h / 2))
end

function winfuncs.right_half()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x + screenrect.w / 2,
        y = screenrect.y,
        w = screenrect.w / 2,
        h = screenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4 * 3, screenrect.y + screenrect.h / 2))
end

function winfuncs.hide()
    local win = window.focusedwindow()
    win:application():hide()
end


function winfuncs.pushwindow_nextscreen()
    local win = window.focusedwindow()
    local screen = win:screen():next()
    local winrect = win:frame()
    local screenrect = screen:frame_without_dock_or_menu()

    -- todo: make the screen move custom configured so we don't have to do this maximized
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w, -- winrect.w,
        h = screenrect.h, -- winrect.h,
    }

    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 2, screenrect.y + screenrect.h / 2))
end

function winfuncs.pushwindow_prevscreen()
    local win = window.focusedwindow()
    local screen = win:screen():previous()
    local winrect = win:frame()
    local screenrect = screen:frame_without_dock_or_menu()

    -- todo: make the screen move custom configured so we don't have to do this maximized
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w, -- winrect.w,
        h = screenrect.h, -- winrect.h,
    }

    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 2, screenrect.y + screenrect.h / 2))
end

function winfuncs.top_left()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y,
        w = screenrect.w / 2,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4, screenrect.y + screenrect.h / 4))
end

function winfuncs.bottom_left()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x,
        y = screenrect.y + screenrect.h / 2,
        w = screenrect.w / 2,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4, screenrect.y + screenrect.h / 4 * 3))
end

function winfuncs.top_right()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x + screenrect.w / 2,
        y = screenrect.y,
        w = screenrect.w / 2,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4 * 3, screenrect.y + screenrect.h / 4))
end

function winfuncs.bottom_right()
    local win = window.focusedwindow()
    local screenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = screenrect.x + screenrect.w / 2,
        y = screenrect.y + screenrect.h / 2,
        w = screenrect.w / 2,
        h = screenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(screenrect.x + screenrect.w / 4 * 3, screenrect.y + screenrect.h / 4 * 3))
end


hotkey.bind(ca, 'RIGHT', winfuncs.pushwindow_nextscreen)
hotkey.bind(ca, 'LEFT', winfuncs.pushwindow_prevscreen)

hotkey.bind(cmd, 'UP', winfuncs.maximize_window)
hotkey.bind(cmd, 'LEFT', winfuncs.left_half)
hotkey.bind(cmd, 'RIGHT', winfuncs.right_half)
hotkey.bind(cmd, 'DOWN', winfuncs.hide)

hotkey.bind(cmd,  'PAD1', winfuncs.bottom_left)
hotkey.bind(cmd,  'PAD2', winfuncs.bottom_half)
hotkey.bind(cmd,  'PAD3', winfuncs.bottom_right)

hotkey.bind(cmd,  'PAD4', winfuncs.left_half)
hotkey.bind(cmd,  'PAD5', winfuncs.maximize_window)
hotkey.bind(cmd,  'PAD6', winfuncs.right_half)

hotkey.bind(cmd,  'PAD7', winfuncs.top_left)
hotkey.bind(cmd,  'PAD8', winfuncs.top_half)
hotkey.bind(cmd,  'PAD9', winfuncs.top_right)

