"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_JQuery = require("../Control.Monad.Eff.JQuery");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var Control_Monad_ST = require("../Control.Monad.ST");
var DOM = require("../DOM");
var DOM_HTML = require("../DOM.HTML");
var DOM_HTML_Window = require("../DOM.HTML.Window");
var Data_Array = require("../Data.Array");
var Data_Array_Partial = require("../Data.Array.Partial");
var Data_Boolean = require("../Data.Boolean");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Number = require("../Data.Number");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Unit = require("../Data.Unit");
var LinearAlgebra_Matrix = require("../LinearAlgebra.Matrix");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var TransformMatrix = function (x) {
    return x;
};
var RotationVector = function (x) {
    return x;
};
var MatrixToString = function (toString) {
    this.toString = toString;
};
var transformMatrixToString = new MatrixToString(function (v) {
    return "(" + (Data_Maybe.fromMaybe("")(Data_String.stripPrefix(", ")(Data_Foldable.foldl(Data_Foldable.foldableArray)(function (s) {
        return function (vec) {
            return s + Data_Foldable.foldl(Data_Foldable.foldableArray)(function (s$prime) {
                return function (n) {
                    return s$prime + (", " + Data_Show.show(Data_Show.showNumber)(n));
                };
            })("")(vec);
        };
    })("")(LinearAlgebra_Matrix.rows(v)))) + ")");
});
var toString = function (dict) {
    return dict.toString;
};
var rotationVectorToString = new MatrixToString(function (v) {
    return "(" + (Data_Maybe.fromMaybe("")(Data_String.stripPrefix(", ")(Data_Foldable.foldl(Data_Foldable.foldableArray)(function (s) {
        return function (vec) {
            return s + Data_Foldable.foldl(Data_Foldable.foldableArray)(function (s$prime) {
                return function (n) {
                    return s$prime + (", " + Data_Show.show(Data_Show.showNumber)(n));
                };
            })("")(vec);
        };
    })("")(LinearAlgebra_Matrix.rows(v)))) + "deg)");
});
var noTransformation = Data_Maybe.fromMaybe(LinearAlgebra_Matrix.identity(1))(LinearAlgebra_Matrix.fromArray(4)(4)([ 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0 ]));
var transformMatrix = function (a) {
    if (Data_Array.length(a) === 16) {
        return Data_Maybe.fromMaybe(LinearAlgebra_Matrix.identity(1))(LinearAlgebra_Matrix.fromArray(4)(4)(a));
    };
    if (Data_Boolean.otherwise) {
        return noTransformation;
    };
    throw new Error("Failed pattern match at Main line 41, column 1 - line 41, column 51: " + [ a.constructor.name ]);
};
var toTransformMatrix = function (str) {
    if (Data_String.contains("matrix3d(")(str)) {
        var a = Data_Foldable.foldl(Data_Foldable.foldableArray)(function (ar) {
            return function (s) {
                return Data_Semigroup.append(Data_Semigroup.semigroupArray)(ar)([ Data_Maybe.fromMaybe(0.0)(Data_Number.fromString(s)) ]);
            };
        })([  ])(Data_String.split(", ")(Data_Maybe.fromMaybe("")(Data_String.stripSuffix(")")(Data_Maybe.fromMaybe("")(Data_String.stripPrefix("matrix3d(")(str))))));
        var $47 = Data_Array.length(a) !== 16;
        if ($47) {
            return noTransformation;
        };
        return transformMatrix(a);
    };
    if (Data_Boolean.otherwise) {
        return noTransformation;
    };
    throw new Error("Failed pattern match at Main line 69, column 1 - line 69, column 47: " + [ str.constructor.name ]);
};
var noRotation = Data_Maybe.fromMaybe(LinearAlgebra_Matrix.identity(1))(LinearAlgebra_Matrix.fromArray(4)(1)([ 0.0, 0.0, 0.0, 0.0 ]));
var rotationVector = function (a) {
    if (Data_Array.length(a) === 4) {
        return Data_Maybe.fromMaybe(LinearAlgebra_Matrix.identity(1))(LinearAlgebra_Matrix.fromArray(4)(1)(a));
    };
    if (Data_Boolean.otherwise) {
        return noRotation;
    };
    throw new Error("Failed pattern match at Main line 29, column 1 - line 29, column 49: " + [ a.constructor.name ]);
};
var sum = function (vs) {
    var add = function (v) {
        return function (v1) {
            var a = Data_Array.zipWith(Data_Semiring.add(Data_Semiring.semiringNumber))(LinearAlgebra_Matrix.column(0)(v))(LinearAlgebra_Matrix.column(0)(v1));
            var x = Data_Maybe.fromMaybe(0.0)(Data_Array.index(a)(0));
            var y = Data_Maybe.fromMaybe(0.0)(Data_Array.index(a)(1));
            return rotationVector(Data_Maybe.fromMaybe([  ])(Data_Array.updateAt(3)($$Math.sqrt(x * x + y * y))(a)));
        };
    };
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (acc) {
        return function (v) {
            return add(acc)(v);
        };
    })(noRotation)(vs);
};
var multiply = function (v) {
    return function (v1) {
        return LinearAlgebra_Matrix.multiply(v)(v1);
    };
};
var rotateCube = function (transformRef) {
    return function (rotation) {
        return function __do() {
            var v = Control_Monad_Eff_JQuery.select(".cube")();
            var v1 = Control_Monad_ST.readSTRef(transformRef)();
            Control_Monad_Eff_JQuery.css({
                transform: "matrix3d" + (toString(transformMatrixToString)(v1) + (" rotate3d" + toString(rotationVectorToString)(multiply(v1)(rotation))))
            })(v)();
            var v2 = Control_Monad_Eff_JQuery.getCss("transform")(v)();
            return toTransformMatrix(v2);
        };
    };
};
var startMouseHandlers = function (transformRef) {
    return function (velocityRef) {
        return function __do() {
            var v = Control_Monad_Eff_JQuery.body();
            var v1 = Control_Monad_ST.newSTRef({
                x: 0.0, 
                y: 0.0
            })();
            var downHandler = function (event) {
                return function (jq) {
                    return function __do() {
                        var v2 = Control_Monad_Eff_JQuery.getPageX(event)();
                        var v3 = Control_Monad_Eff_JQuery.getPageY(event)();
                        Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v1)({
                            x: v2, 
                            y: v3
                        }))();
                        var v4 = Control_Monad_ST.newSTRef(true)();
                        var moveHandler = function (event$prime) {
                            return function (jq$prime) {
                                return function __do() {
                                    var v5 = Control_Monad_Eff_JQuery.getPageX(event$prime)();
                                    var v6 = Control_Monad_Eff_JQuery.getPageY(event$prime)();
                                    Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(v1)({
                                        x: v5, 
                                        y: v6
                                    }))();
                                    var dx = -(v6 - v3);
                                    var dy = v5 - v2;
                                    var rotation = rotationVector([ dx, dy, 0.0, $$Math.sqrt(dx * dx + dy * dy) * 0.4 ]);
                                    return rotateCube(transformRef)(rotation)();
                                };
                            };
                        };
                        var upHandler = function (event$prime) {
                            return function (jq$prime) {
                                return function __do() {
                                    var v5 = Control_Monad_Eff_JQuery.select(".cube")();
                                    Control_Monad_Eff_JQuery.off("mousemove")(v)();
                                    var v6 = Control_Monad_Eff_JQuery.getCss("transform")(v5)();
                                    Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(transformRef)(toTransformMatrix(v6)))();
                                    return Control_Monad_ST.writeSTRef(v4)(false)();
                                };
                            };
                        };
                        Control_Monad_Eff_JQuery.on("mousemove")(moveHandler)(v)();
                        return Control_Monad_Eff_JQuery.on("mouseup")(upHandler)(v)();
                    };
                };
            };
            return Control_Monad_Eff_JQuery.on("mousedown")(downHandler)(v)();
        };
    };
};
var drawCube = function __do() {
    var v = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("front_face")(v)();
    Control_Monad_Eff_JQuery.addClass("face")(v)();
    var v1 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("back_face")(v1)();
    Control_Monad_Eff_JQuery.addClass("face")(v1)();
    var v2 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("right_face")(v2)();
    Control_Monad_Eff_JQuery.addClass("face")(v2)();
    var v3 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("left_face")(v3)();
    Control_Monad_Eff_JQuery.addClass("face")(v3)();
    var v4 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("top_face")(v4)();
    Control_Monad_Eff_JQuery.addClass("face")(v4)();
    var v5 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.setAttr("id")("bottom_face")(v5)();
    Control_Monad_Eff_JQuery.addClass("face")(v5)();
    var v6 = Control_Monad_Eff_JQuery.create("<figure>")();
    Control_Monad_Eff_JQuery.addClass("cube")(v6)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateX(-100px) translateY(-100px) translateZ(100px)"
    })(v)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateX(-100px) translateY(-100px) translateZ(-100px)"
    })(v1)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateY(-100px) rotateY(90deg)"
    })(v2)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateY(-100px) translateX(-200px) rotateY(90deg)"
    })(v3)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateX(-100px) translateY(-200px) rotateX(90deg)"
    })(v4)();
    Control_Monad_Eff_JQuery.css({
        transform: "translateX(-100px) rotateX(90deg)"
    })(v5)();
    Control_Monad_Eff_JQuery.css({
        position: "relative", 
        transformStyle: "preserve-3d"
    })(v6)();
    Control_Monad_Eff_JQuery.append(v)(v6)();
    Control_Monad_Eff_JQuery.append(v1)(v6)();
    Control_Monad_Eff_JQuery.append(v2)(v6)();
    Control_Monad_Eff_JQuery.append(v3)(v6)();
    Control_Monad_Eff_JQuery.append(v4)(v6)();
    Control_Monad_Eff_JQuery.append(v5)(v6)();
    var v7 = Control_Monad_Eff_JQuery.create("<div>")();
    Control_Monad_Eff_JQuery.setAttr("id")("cube-wrapper")(v7)();
    Control_Monad_Eff_JQuery.css({
        position: "absolute", 
        left: "50%", 
        top: "50%", 
        perspective: "1500px"
    })(v7)();
    Control_Monad_Eff_JQuery.append(v6)(v7)();
    var v8 = Control_Monad_Eff_JQuery.body();
    Control_Monad_Eff_JQuery.append(v7)(v8)();
    Control_Monad_Eff_JQuery.css({
        width: "100%", 
        height: "100%"
    })(v8)();
    var v9 = Control_Monad_Eff_JQuery.select(".face")();
    Control_Monad_Eff_JQuery.css({
        position: "absolute", 
        width: "200px", 
        height: "200px", 
        border: "solid black 3px"
    })(v9)();
    return Control_Monad_Eff_JQuery.css({
        transform: "rotateX(-45deg)rotateY(45deg)"
    })(v6)();
};
var changeSpeed = function (s) {
    return function (v) {
        var x = Data_Maybe.fromMaybe(0.0)(LinearAlgebra_Matrix.element(0)(0)(v));
        var y = Data_Maybe.fromMaybe(0.0)(LinearAlgebra_Matrix.element(1)(0)(v));
        var a = (function () {
            var $77 = y === 0.0;
            if ($77) {
                return 0.0;
            };
            var $78 = x === 0.0;
            if ($78) {
                return $$Math.pi / 2.0;
            };
            return $$Math.atan($$Math.abs(y / x));
        })();
        return rotationVector([ s * $$Math.cos(a) * (function () {
            var $79 = x < 0.0;
            if ($79) {
                return -1.0;
            };
            return 1.0;
        })(), s * $$Math.sin(a) * (function () {
            var $80 = y < 0.0;
            if ($80) {
                return -1.0;
            };
            return 1.0;
        })(), 0.0, s ]);
    };
};
var average = function (vs) {
    var v = sum(vs);
    var s = Data_Maybe.fromMaybe(0.0)(LinearAlgebra_Matrix.element(3)(0)(v)) / Data_Int.toNumber(Data_Array.length(vs));
    return changeSpeed(s)(v);
};
var startSpeedometer = function (velocityRef) {
    return function (mousePosRef) {
        return function (runFlagRef) {
            var looper = function (prevPos) {
                return function (velocities) {
                    var speedometer = function __do() {
                        var v = Control_Monad_ST.readSTRef(mousePosRef)();
                        var r = rotationVector([ -(v.y - prevPos.y) * 0.4, (v.x - prevPos.x) * 0.4, 0.0, 0.0 ]);
                        var newVels = Data_Array.snoc(Data_Array_Partial.tail()(velocities))(r);
                        var v1 = Control_Monad_ST.readSTRef(runFlagRef)();
                        if (v1) {
                            return looper(v)(newVels)();
                        };
                        var v2 = Control_Monad_ST.readSTRef(velocityRef)();
                        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(velocityRef)(sum([ average(newVels), v2 ])))();
                    };
                    return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_Eff_Timer.setTimeout(25)(speedometer));
                };
            };
            return function __do() {
                var v = Control_Monad_ST.readSTRef(mousePosRef)();
                return looper(v)(Data_Array.replicate(5)(noRotation))();
            };
        };
    };
};
var angle = function (v) {
    return Data_Maybe.fromMaybe(0.0)(LinearAlgebra_Matrix.element(3)(0)(v));
};
var startSpinner = function (transformRef) {
    return function (velocityRef) {
        var spinner = function __do() {
            var v = Control_Monad_ST.readSTRef(velocityRef)();
            (function () {
                var $89 = angle(v) !== 0.0;
                if ($89) {
                    return function __do() {
                        var v1 = rotateCube(transformRef)(v)();
                        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Monad_ST.writeSTRef(transformRef)(v1))();
                    };
                };
                return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
            })()();
            var v1 = DOM_HTML.window();
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(DOM_HTML_Window.requestAnimationFrame(spinner)(v1))();
        };
        return spinner;
    };
};
var run = function __do() {
    var v = Control_Monad_Eff_JQuery.select(".cube")();
    var v1 = Control_Monad_Eff_JQuery.getCss("transform")(v)();
    Control_Monad_Eff_Console.log(v1)();
    var v2 = Control_Monad_ST.newSTRef(toTransformMatrix(v1))();
    var v3 = Control_Monad_ST.newSTRef(noRotation)();
    startSpinner(v2)(v3)();
    startMouseHandlers(v2)(v3)();
    return Data_Unit.unit;
};
var main = function __do() {
    drawCube();
    return run();
};
module.exports = {
    RotationVector: RotationVector, 
    TransformMatrix: TransformMatrix, 
    MatrixToString: MatrixToString, 
    angle: angle, 
    average: average, 
    changeSpeed: changeSpeed, 
    drawCube: drawCube, 
    main: main, 
    multiply: multiply, 
    noRotation: noRotation, 
    noTransformation: noTransformation, 
    rotateCube: rotateCube, 
    rotationVector: rotationVector, 
    run: run, 
    startMouseHandlers: startMouseHandlers, 
    startSpeedometer: startSpeedometer, 
    startSpinner: startSpinner, 
    sum: sum, 
    toString: toString, 
    toTransformMatrix: toTransformMatrix, 
    transformMatrix: transformMatrix, 
    transformMatrixToString: transformMatrixToString, 
    rotationVectorToString: rotationVectorToString
};
