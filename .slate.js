// generic operations
var fullscreen = slate.operation("move", {
    "x": "screenOriginX",
    "y": "screenOriginY",
    "width": "screenSizeX",
    "height": "screenSizeY"
});

var leftHalf = slate.operation("move", {
    "x": "screenOriginX",
    "y": "screenOriginY",
    "width": "screenSizeX/2",
    "height": "screenSizeY"
})
var rightHalf = leftHalf.dup({"x": "screenOriginX+screenSizeX/2"})

var topLeft = slate.operation("corner", {
    "direction": "top-left",
    "width": "screenSizeX/2",
    "height": "screenSizeY/2"
})
var topRight = topLeft.dup({"direction": "top-right"})
var bottomLeft = topLeft.dup({"direction": "bottom-left"})
var bottomRight = topLeft.dup({"direction": "bottom-right"})

var topHalf = slate.operation("push", {
    "direction": "top",
    "style": "bar-resize:screenSizeY/2"
})
var bottomHalf = topHalf.dup({"direction": "bottom"})

var moveScreenLeft = slate.operation("move", {
    "x": "screenOriginX",
    "y": "screenOriginY",
    "width": "windowSizeX",
    "height": "windowSizeY",
    "screen": "left"
})
var moveScreenRight = moveScreenLeft.dup({"screen": "right"})

// key bindings
function doFullscreen(win) { win.doOperation(fullscreen) }
slate.bind("pad5", doFullscreen);
slate.bind("up:cmd", doFullscreen);

function doLeftHalf(win) { win.doOperation(leftHalf) }
slate.bind("left:cmd", doLeftHalf);
slate.bind("pad4", doLeftHalf);

function doRightHalf(win) { win.doOperation(rightHalf) }
slate.bind("right:cmd", doRightHalf);
slate.bind("pad6", doRightHalf);

slate.bind("pad7", function(win) { win.doOperation(topLeft) })
slate.bind("pad9", function(win) { win.doOperation(topRight) })
slate.bind("pad1", function(win) { win.doOperation(bottomLeft) })
slate.bind("pad3", function(win) { win.doOperation(bottomRight) })

slate.bind("pad2", function(win) { win.doOperation(bottomHalf) })
slate.bind("pad8", function(win) { win.doOperation(topHalf) })

slate.bind("left:alt;ctrl", function(win) { win.doOperation(moveScreenLeft) })
slate.bind("right:alt;ctrl", function(win) { win.doOperation(moveScreenRight) })
