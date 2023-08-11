export function src(media) {
  return function () {
    return media.src;
  };
}

export function setSrc(src) {
  return function (media) {
    return function () {
      media.src = src;
    };
  };
}

// ----------------------------------------------------------------------------

export function currentSrc(media) {
  return function () {
    return media.currentSrc;
  };
}

// ----------------------------------------------------------------------------

export function crossOrigin(media) {
  return function () {
    return media.crossOrigin;
  };
}

export function setCrossOrigin(crossOrigin) {
  return function (media) {
    return function () {
      media.crossOrigin = crossOrigin;
    };
  };
}

// ----------------------------------------------------------------------------

export function _networkState(media) {
  return media.networkState;
}

// ----------------------------------------------------------------------------

export function preload(media) {
  return function () {
    return media.preload;
  };
}

export function setPreload(preload) {
  return function (media) {
    return function () {
      media.preload = preload;
    };
  };
}

// ----------------------------------------------------------------------------

export function load(media) {
  return function () {
    return media.load();
  };
}

// ----------------------------------------------------------------------------

export function _canPlayType(type, media) {
  return media.canPlayType(type);
}

// ----------------------------------------------------------------------------

export function _readyState(media) {
  return media.readyState;
}

// ----------------------------------------------------------------------------

export function seeking(media) {
  return function () {
    return media.seeking;
  };
}

// ----------------------------------------------------------------------------

export function currentTime(media) {
  return function () {
    return media.currentTime;
  };
}

export function setCurrentTime(currentTime) {
  return function (media) {
    return function () {
      media.currentTime = currentTime;
    };
  };
}

// ----------------------------------------------------------------------------

export function duration(media) {
  return function () {
    return media.duration;
  };
}

// ----------------------------------------------------------------------------

export function getStartDate(media) {
  return function () {
    return media.getStartDate();
  };
}

// ----------------------------------------------------------------------------

export function paused(media) {
  return function () {
    return media.paused;
  };
}

// ----------------------------------------------------------------------------

export function defaultPlaybackRate(media) {
  return function () {
    return media.defaultPlaybackRate;
  };
}

export function setDefaultPlaybackRate(defaultPlaybackRate) {
  return function (media) {
    return function () {
      media.defaultPlaybackRate = defaultPlaybackRate;
    };
  };
}

// ----------------------------------------------------------------------------

export function playbackRate(media) {
  return function () {
    return media.playbackRate;
  };
}

export function setPlaybackRate(playbackRate) {
  return function (media) {
    return function () {
      media.playbackRate = playbackRate;
    };
  };
}

// ----------------------------------------------------------------------------

export function ended(media) {
  return function () {
    return media.ended;
  };
}

// ----------------------------------------------------------------------------

export function autoplay(media) {
  return function () {
    return media.autoplay;
  };
}

export function setAutoplay(autoplay) {
  return function (media) {
    return function () {
      media.autoplay = autoplay;
    };
  };
}

// ----------------------------------------------------------------------------

export function loop(media) {
  return function () {
    return media.loop;
  };
}

export function setLoop(loop) {
  return function (media) {
    return function () {
      media.loop = loop;
    };
  };
}

// ----------------------------------------------------------------------------

export function play(media) {
  return function () {
    media.play();
  };
}

// ----------------------------------------------------------------------------

export function pause(media) {
  return function () {
    media.pause();
  };
}

// ----------------------------------------------------------------------------

export function mediaGroup(media) {
  return function () {
    return media.mediaGroup;
  };
}

export function setMediaGroup(mediaGroup) {
  return function (media) {
    return function () {
      media.mediaGroup = mediaGroup;
    };
  };
}

// ----------------------------------------------------------------------------

export function controls(media) {
  return function () {
    return media.controls;
  };
}

export function setControls(controls) {
  return function (media) {
    return function () {
      media.controls = controls;
    };
  };
}

// ----------------------------------------------------------------------------

export function volume(media) {
  return function () {
    return media.volume;
  };
}

export function setVolume(volume) {
  return function (media) {
    return function () {
      media.volume = volume;
    };
  };
}

// ----------------------------------------------------------------------------

export function muted(media) {
  return function () {
    return media.muted;
  };
}

export function setMuted(muted) {
  return function (media) {
    return function () {
      media.muted = muted;
    };
  };
}

// ----------------------------------------------------------------------------

export function defaultMuted(media) {
  return function () {
    return media.defaultMuted;
  };
}

export function setDefaultMuted(defaultMuted) {
  return function (media) {
    return function () {
      media.defaultMuted = defaultMuted;
    };
  };
}
