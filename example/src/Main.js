"use strict";

exports.onLoad = function (action) {
  return function () {
    window.addEventListener("load", function () {
      action();
    });
  };
};