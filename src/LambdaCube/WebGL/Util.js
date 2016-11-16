/* global exports */
"use strict";

// module Util

exports.setFloatArray
  = function setFloatArray(ta) {
      return function(a) {
        return function(){ta.set(a);};
      };
     };

exports.setIntArray
  = function setIntArray(ta) {
      return function(a) {
        return function(){ta.set(a);};
      };
     };

exports.newArrayBuffer
  = function newArrayBuffer(s) {
      return function() {return new ArrayBuffer(s);};
     };

exports.newWord8View
  = function newWord8View(b) {
      return function(o) {
        return function(l) {
          return function(){return new Uint8Array(b,o,l);};
        };
      };
     };

exports.newWord16View
  = function newWord16View(b) {
      return function(o) {
        return function(l) {
          return function(){return new Uint16Array(b,o,l);};
        };
      };
     };

exports.newInt8View
  = function newInt8View(b) {
      return function(o) {
        return function(l) {
          return function(){return new Int8Array(b,o,l);};
        };
      };
     };

exports.newInt16View
  = function newInt16View(b) {
      return function(o) {
        return function(l) {
          return function(){return new Int16Array(b,o,l);};
        };
      };
     };

exports.newFloatView
  = function newFloatView(b) {
      return function(o) {
        return function(l) {
          return function(){return new Float32Array(b,o,l);};
        };
      };
     };

exports.setArrayView
  = function setArrayView(av) {
      return function(a) {
        return function(){av.set(a);};
      };
     };

exports.nullWebGLBuffer = null;

exports.bufferDataAlloc
  = function bufferDataAlloc(target)
   {return function(size)
    {return function(usage)
     {return function()
      {gl.bufferData(target,size,usage);};};};};

exports.bufferSubDataArrayBuffer
  = function bufferSubDataArrayBuffer(target)
   {return function(offset)
    {return function(data)
     {return function()
      {gl.bufferSubData(target,offset,data);};};};};

exports.bufferSubDataArrayView
  = function bufferSubDataArrayView(target)
   {return function(offset)
    {return function(data)
     {return function()
      {gl.bufferSubData(target,offset,data);};};};};

exports.texImage2DNull_
  = function texImage2DNull_(target)
   {return function(level)
    {return function(internalformat)
     {return function(width)
      {return function(height)
       {return function(border)
        {return function(format)
         {return function(type)
           {return function()
            {gl.texImage2D(target,level,internalformat,width,height,border,format,type,null);};};};};};};};};};

exports.nullWebGLTexture = null;

exports.loadImage_
  = function loadImage_(name)
    {return function(continuation)
     {return function()
      {var i = new Image();
       i.src = name;
       i.onload = continuation (i);
      };};};

exports.texImage2D__
  = function (target)
    {return function(level)
     {return function(internalformat)
      {return function(format)
       {return function(type)
        {return function(pixels)
         {return function()
          {gl.texImage2D(target,level,internalformat,format,type,pixels);};};};};};};};
