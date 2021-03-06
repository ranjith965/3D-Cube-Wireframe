// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_File_Types = require("../DOM.File.Types");
var DOM_HTML_SelectionMode = require("../DOM.HTML.SelectionMode");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Functor = require("../Data.Functor");
var Data_JSDate = require("../Data.JSDate");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var stepUp = $foreign["stepUp'"](1);
var stepDown = $foreign["stepDown'"](1);
var list = function ($0) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._list($0));
};
var form = function ($1) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._form($1));
};
var files = function ($2) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._files($2));
};
module.exports = {
    files: files, 
    form: form, 
    list: list, 
    accept: $foreign.accept, 
    alt: $foreign.alt, 
    autocomplete: $foreign.autocomplete, 
    autofocus: $foreign.autofocus, 
    checkValidity: $foreign.checkValidity, 
    checked: $foreign.checked, 
    defaultChecked: $foreign.defaultChecked, 
    defaultValue: $foreign.defaultValue, 
    dirName: $foreign.dirName, 
    disabled: $foreign.disabled, 
    formAction: $foreign.formAction, 
    formEnctype: $foreign.formEnctype, 
    formMethod: $foreign.formMethod, 
    formNoValidate: $foreign.formNoValidate, 
    formTarget: $foreign.formTarget, 
    height: $foreign.height, 
    indeterminate: $foreign.indeterminate, 
    labels: $foreign.labels, 
    max: $foreign.max, 
    maxLength: $foreign.maxLength, 
    min: $foreign.min, 
    minLength: $foreign.minLength, 
    multiple: $foreign.multiple, 
    name: $foreign.name, 
    pattern: $foreign.pattern, 
    placeholder: $foreign.placeholder, 
    readOnly: $foreign.readOnly, 
    required: $foreign.required, 
    select: $foreign.select, 
    selectionDirection: $foreign.selectionDirection, 
    selectionEnd: $foreign.selectionEnd, 
    selectionStart: $foreign.selectionStart, 
    setAccept: $foreign.setAccept, 
    setAlt: $foreign.setAlt, 
    setAutocomplete: $foreign.setAutocomplete, 
    setAutofocus: $foreign.setAutofocus, 
    setChecked: $foreign.setChecked, 
    setCustomValidity: $foreign.setCustomValidity, 
    setDefaultChecked: $foreign.setDefaultChecked, 
    setDefaultValue: $foreign.setDefaultValue, 
    setDirName: $foreign.setDirName, 
    setDisabled: $foreign.setDisabled, 
    setFormAction: $foreign.setFormAction, 
    setFormEnctype: $foreign.setFormEnctype, 
    setFormMethod: $foreign.setFormMethod, 
    setFormNoValidate: $foreign.setFormNoValidate, 
    setFormTarget: $foreign.setFormTarget, 
    setHeight: $foreign.setHeight, 
    setIndeterminate: $foreign.setIndeterminate, 
    setMax: $foreign.setMax, 
    setMaxLength: $foreign.setMaxLength, 
    setMin: $foreign.setMin, 
    setMinLength: $foreign.setMinLength, 
    setMultiple: $foreign.setMultiple, 
    setName: $foreign.setName, 
    setPattern: $foreign.setPattern, 
    setPlaceholder: $foreign.setPlaceholder, 
    setRangeText: $foreign.setRangeText, 
    "setRangeText'": $foreign["setRangeText'"], 
    setReadOnly: $foreign.setReadOnly, 
    setRequired: $foreign.setRequired, 
    setSelectionDirection: $foreign.setSelectionDirection, 
    setSelectionEnd: $foreign.setSelectionEnd, 
    setSelectionRange: $foreign.setSelectionRange, 
    setSelectionStart: $foreign.setSelectionStart, 
    setSize: $foreign.setSize, 
    setSrc: $foreign.setSrc, 
    setStep: $foreign.setStep, 
    setType: $foreign.setType, 
    setValue: $foreign.setValue, 
    setValueAsDate: $foreign.setValueAsDate, 
    setValueAsNumber: $foreign.setValueAsNumber, 
    setWidth: $foreign.setWidth, 
    size: $foreign.size, 
    src: $foreign.src, 
    step: $foreign.step, 
    "stepDown'": $foreign["stepDown'"], 
    "stepUp'": $foreign["stepUp'"], 
    type_: $foreign.type_, 
    validationMessage: $foreign.validationMessage, 
    validity: $foreign.validity, 
    value: $foreign.value, 
    valueAsDate: $foreign.valueAsDate, 
    valueAsNumber: $foreign.valueAsNumber, 
    width: $foreign.width, 
    willValidate: $foreign.willValidate
};
