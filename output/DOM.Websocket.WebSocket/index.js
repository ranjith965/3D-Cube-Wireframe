// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_File_Types = require("../DOM.File.Types");
var DOM_Websocket_BinaryType = require("../DOM.Websocket.BinaryType");
var DOM_Websocket_Event_Types = require("../DOM.Websocket.Event.Types");
var DOM_Websocket_ReadyState = require("../DOM.Websocket.ReadyState");
var DOM_Websocket_Types = require("../DOM.Websocket.Types");
var Data_ArrayBuffer_Types = require("../Data.ArrayBuffer.Types");
var Data_Foreign = require("../Data.Foreign");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var setBinaryType = function (ws) {
    return function ($9) {
        return $foreign.setBinaryTypeImpl(ws)(DOM_Websocket_BinaryType.printBinaryType($9));
    };
};
var sendString = function (ws) {
    return function ($10) {
        return $foreign.sendImpl(ws)(Data_Foreign.toForeign($10));
    };
};
var sendBlob = function (ws) {
    return function ($11) {
        return $foreign.sendImpl(ws)(Data_Foreign.toForeign($11));
    };
};
var sendArrayBufferView = function (ws) {
    return function ($12) {
        return $foreign.sendImpl(ws)(Data_Foreign.toForeign($12));
    };
};
var sendArrayBuffer = function (ws) {
    return function ($13) {
        return $foreign.sendImpl(ws)(Data_Foreign.toForeign($13));
    };
};
var readyState = function (ws) {
    return function __do() {
        var v = $foreign.readyStateImpl(ws)();
        return Data_Maybe.fromJust()(DOM_Websocket_ReadyState.toEnumReadyState(v));
    };
};
var getBinaryType = function (ws) {
    return Data_Functor.mapFlipped(Control_Monad_Eff.functorEff)($foreign.getBinaryTypeImpl(ws))(function (v) {
        var __unused = function (dictPartial1) {
            return function ($dollar4) {
                return $dollar4;
            };
        };
        return __unused()((function () {
            if (v === "blob") {
                return DOM_Websocket_BinaryType.Blob.value;
            };
            if (v === "arraybuffer") {
                return DOM_Websocket_BinaryType["ArrayBuffer"].value;
            };
            throw new Error("Failed pattern match at DOM.Websocket.WebSocket line 61, column 28 - line 63, column 33: " + [ v.constructor.name ]);
        })());
    });
};
module.exports = {
    getBinaryType: getBinaryType, 
    readyState: readyState, 
    sendArrayBuffer: sendArrayBuffer, 
    sendArrayBufferView: sendArrayBufferView, 
    sendBlob: sendBlob, 
    sendString: sendString, 
    setBinaryType: setBinaryType, 
    bufferedAmount: $foreign.bufferedAmount, 
    close: $foreign.close, 
    create: $foreign.create, 
    extensions: $foreign.extensions, 
    protocol: $foreign.protocol, 
    url: $foreign.url
};
