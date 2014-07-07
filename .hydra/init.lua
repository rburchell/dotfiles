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
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y,
        w = gscreenrect.w,
        h = gscreenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 2, gscreenrect.y + gscreenrect.h / 2))
end

function winfuncs.top_half()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y,
        w = gscreenrect.w,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 2, gscreenrect.y + gscreenrect.h / 4))
end

function winfuncs.bottom_half()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y + gscreenrect.h / 2,
        w = gscreenrect.w,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 2, gscreenrect.y + gscreenrect.h / 4 * 3))
end

function winfuncs.left_half()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y,
        w = gscreenrect.w / 2,
        h = gscreenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4, gscreenrect.y + gscreenrect.h / 2))
end

function winfuncs.right_half()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x + gscreenrect.w / 2,
        y = gscreenrect.y,
        w = gscreenrect.w / 2,
        h = gscreenrect.h,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4 * 3, gscreenrect.y + gscreenrect.h / 2))
end

function winfuncs.hide()
    local win = window.focusedwindow()
    win:application():hide()
end


function winfuncs.pushwindow_nextscreen()
    local win = window.focusedwindow()
    local screen = win:screen()
    local nscreen = win:screen():next()
    local winrect = win:frame()
    local gscreenrect = screen:frame_without_dock_or_menu()
    local gnscreenrect = nscreen:frame_without_dock_or_menu()

    -- take current position and size (gscreenrect, winrect) and figure out the new frame
    -- coordinates to use on gnscreenrect.
    --
    -- winrect.x - gscreenrect.x because we want to get the relative X
    -- coordinate, then we divide by screen width to get the multiplier..
    local xPosMul = ((winrect.x - gscreenrect.x) / gscreenrect.w)
    local yPosMul = ((winrect.y - gscreenrect.y) / gscreenrect.h)
    local widthMul = winrect.w / gscreenrect.w
    local heightMul = winrect.h / gscreenrect.h

    local newframe = {
        x = gnscreenrect.x + gnscreenrect.w * xPosMul,
        y = gnscreenrect.y + gnscreenrect.h * yPosMul,
        w = gnscreenrect.w * widthMul,
        h = gnscreenrect.h * heightMul, -- winrect.h,
    }

    win:setframe(newframe)
    mouse.set(geometry.point(newframe.x + newframe.w / 2, newframe.y + newframe.h / 2))
end

function winfuncs.pushwindow_prevscreen()
    local win = window.focusedwindow()
    local screen = win:screen()
    local pscreen = win:screen():previous()
    local winrect = win:frame()
    local gscreenrect = screen:frame_without_dock_or_menu()
    local gpscreenrect = pscreen:frame_without_dock_or_menu()

    -- take current position and size (gscreenrect, winrect) and figure out the new frame
    -- coordinates to use on gpscreenrect.
    --
    -- winrect.x - gscreenrect.x because we want to get the relative X
    -- coordinate, then we divide by screen width to get the multiplier..
    local xPosMul = ((winrect.x - gscreenrect.x) / gscreenrect.w)
    local yPosMul = ((winrect.y - gscreenrect.y) / gscreenrect.h)
    local widthMul = winrect.w / gscreenrect.w
    local heightMul = winrect.h / gscreenrect.h

    local newframe = {
        x = gpscreenrect.x + gpscreenrect.w * xPosMul,
        y = gpscreenrect.y + gpscreenrect.h * yPosMul,
        w = gpscreenrect.w * widthMul,
        h = gpscreenrect.h * heightMul, -- winrect.h,
    }

    win:setframe(newframe)
    mouse.set(geometry.point(newframe.x + newframe.w / 2, newframe.y + newframe.h / 2))
end

function winfuncs.top_left()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y,
        w = gscreenrect.w / 2,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4, gscreenrect.y + gscreenrect.h / 4))
end

function winfuncs.bottom_left()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x,
        y = gscreenrect.y + gscreenrect.h / 2,
        w = gscreenrect.w / 2,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4, gscreenrect.y + gscreenrect.h / 4 * 3))
end

function winfuncs.top_right()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x + gscreenrect.w / 2,
        y = gscreenrect.y,
        w = gscreenrect.w / 2,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4 * 3, gscreenrect.y + gscreenrect.h / 4))
end

function winfuncs.bottom_right()
    local win = window.focusedwindow()
    local gscreenrect = win:screen():frame_without_dock_or_menu()
    local newframe = {
        x = gscreenrect.x + gscreenrect.w / 2,
        y = gscreenrect.y + gscreenrect.h / 2,
        w = gscreenrect.w / 2,
        h = gscreenrect.h / 2,
    }
    win:setframe(newframe)
    mouse.set(geometry.point(gscreenrect.x + gscreenrect.w / 4 * 3, gscreenrect.y + gscreenrect.h / 4 * 3))
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

