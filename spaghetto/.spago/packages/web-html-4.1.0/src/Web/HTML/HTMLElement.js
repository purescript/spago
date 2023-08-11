export function _read(nothing, just, value) {
  var tag = Object.prototype.toString.call(value);
  if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
    return just(value);
  } else {
    return nothing;
  }
}

// ----------------------------------------------------------------------------

export function title(elt) {
  return function () {
    return elt.title;
  };
}

export function setTitle(title) {
  return function (elt) {
    return function () {
      elt.title = title;
    };
  };
}

// ----------------------------------------------------------------------------

export function lang(elt) {
  return function () {
    return elt.lang;
  };
}

export function setLang(lang) {
  return function (elt) {
    return function () {
      elt.lang = lang;
    };
  };
}

// ----------------------------------------------------------------------------

export function dir(elt) {
  return function () {
    return elt.dir;
  };
}

export function setDir(dir) {
  return function (elt) {
    return function () {
      elt.dir = dir;
    };
  };
}

// ----------------------------------------------------------------------------

export function hidden(elt) {
  return function () {
    return elt.hidden;
  };
}

export function setHidden(hidden) {
  return function (elt) {
    return function () {
      elt.hidden = hidden;
    };
  };
}

// ----------------------------------------------------------------------------

export function tabIndex(elt) {
  return function () {
    return elt.tabIndex;
  };
}

export function setTabIndex(tabIndex) {
  return function (elt) {
    return function () {
      elt.tabIndex = tabIndex;
    };
  };
}

// ----------------------------------------------------------------------------

export function draggable(elt) {
  return function () {
    return elt.draggable;
  };
}

export function setDraggable(draggable) {
  return function (elt) {
    return function () {
      elt.draggable = draggable;
    };
  };
}

// ----------------------------------------------------------------------------

export function contentEditable(elt) {
  return function () {
    return elt.contentEditable;
  };
}

export function setContentEditable(contentEditable) {
  return function (elt) {
    return function () {
      elt.contentEditable = contentEditable;
    };
  };
}

export function isContentEditable(elt) {
  return function () {
    return elt.isContentEditable;
  };
}

// ----------------------------------------------------------------------------

export function spellcheck(elt) {
  return function () {
    return elt.spellcheck;
  };
}

export function setSpellcheck(spellcheck) {
  return function (elt) {
    return function () {
      elt.spellcheck = spellcheck;
    };
  };
}

// ----------------------------------------------------------------------------

export function click(elt) {
  return function () {
    return elt.click();
  };
}

export function focus(elt) {
  return function () {
    return elt.focus();
  };
}

export function blur(elt) {
  return function () {
    return elt.blur();
  };
}

// - CSSOM ---------------------------------------------------------------------

export function _offsetParent(el) {
  return function () {
    return el.offsetParent;
  };
}

export function offsetTop(el) {
  return function () {
    return el.offsetTop;
  };
}

export function offsetLeft(el) {
  return function () {
    return el.offsetLeft;
  };
}

export function offsetWidth(el) {
  return function () {
    return el.offsetWidth;
  };
}

export function offsetHeight(el) {
  return function () {
    return el.offsetHeight;
  };
}
