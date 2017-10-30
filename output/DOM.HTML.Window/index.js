// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_WebStorage_Types = require("../DOM.WebStorage.Types");
var Data_Eq = require("../Data.Eq");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_Nullable = require("../Data.Nullable");
var Data_Ord = require("../Data.Ord");
var Prelude = require("../Prelude");
var RequestIdleCallbackId = function (x) {
    return x;
};
var RequestAnimationFrameId = function (x) {
    return x;
};
var requestIdleCallback = function (opts) {
    return function (fn) {
        return function ($30) {
            return Data_Functor.map(Control_Monad_Eff.functorEff)(RequestIdleCallbackId)($foreign._requestIdleCallback(opts)(fn)($30));
        };
    };
};
var requestAnimationFrame = function (fn) {
    return function ($31) {
        return Data_Functor.map(Control_Monad_Eff.functorEff)(RequestAnimationFrameId)($foreign._requestAnimationFrame(fn)($31));
    };
};
var promptDefault = function (msg) {
    return function (defaultText) {
        return function (window) {
            return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._prompt(msg)(defaultText)(window));
        };
    };
};
var prompt = function (msg) {
    return function (window) {
        return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._prompt(msg)("")(window));
    };
};
var open = function (url$prime) {
    return function (name) {
        return function (features) {
            return function (window) {
                return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._open(url$prime)(name)(features)(window));
            };
        };
    };
};
var newtypeRequestIdleCallbackId = new Data_Newtype.Newtype(function (n) {
    return n;
}, RequestIdleCallbackId);
var newtypeRequestAnimationFrameId = new Data_Newtype.Newtype(function (n) {
    return n;
}, RequestAnimationFrameId);
var eqRequestIdleCallbackId = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordRequestIdleCallbackId = new Data_Ord.Ord(function () {
    return eqRequestIdleCallbackId;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
var eqRequestAnimationFrameId = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordRequestAnimationFrameId = new Data_Ord.Ord(function () {
    return eqRequestAnimationFrameId;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordInt)(x)(y);
    };
});
var cancelIdleCallback = function (idAF) {
    return $foreign._cancelIdleCallback(Data_Newtype.unwrap(newtypeRequestIdleCallbackId)(idAF));
};
var cancelAnimationFrame = function (idAF) {
    return $foreign._cancelAnimationFrame(Data_Newtype.unwrap(newtypeRequestAnimationFrameId)(idAF));
};
module.exports = {
    cancelAnimationFrame: cancelAnimationFrame, 
    cancelIdleCallback: cancelIdleCallback, 
    open: open, 
    prompt: prompt, 
    promptDefault: promptDefault, 
    requestAnimationFrame: requestAnimationFrame, 
    requestIdleCallback: requestIdleCallback, 
    newtypeRequestAnimationFrameId: newtypeRequestAnimationFrameId, 
    eqRequestAnimationFrameId: eqRequestAnimationFrameId, 
    ordRequestAnimationFrameId: ordRequestAnimationFrameId, 
    newtypeRequestIdleCallbackId: newtypeRequestIdleCallbackId, 
    eqRequestIdleCallbackId: eqRequestIdleCallbackId, 
    ordRequestIdleCallbackId: ordRequestIdleCallbackId, 
    alert: $foreign.alert, 
    confirm: $foreign.confirm, 
    document: $foreign.document, 
    history: $foreign.history, 
    innerHeight: $foreign.innerHeight, 
    innerWidth: $foreign.innerWidth, 
    localStorage: $foreign.localStorage, 
    location: $foreign.location, 
    moveBy: $foreign.moveBy, 
    moveTo: $foreign.moveTo, 
    navigator: $foreign.navigator, 
    outerHeight: $foreign.outerHeight, 
    outerWidth: $foreign.outerWidth, 
    print: $foreign.print, 
    resizeBy: $foreign.resizeBy, 
    resizeTo: $foreign.resizeTo, 
    screenX: $foreign.screenX, 
    screenY: $foreign.screenY, 
    scroll: $foreign.scroll, 
    scrollBy: $foreign.scrollBy, 
    scrollX: $foreign.scrollX, 
    scrollY: $foreign.scrollY, 
    sessionStorage: $foreign.sessionStorage, 
    url: $foreign.url
};
