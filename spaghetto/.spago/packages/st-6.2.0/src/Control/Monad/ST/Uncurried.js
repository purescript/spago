export const mkSTFn1 = function mkSTFn1(fn) {
  return function(x) {
    return fn(x)();
  };
};
  
export const mkSTFn2 = function mkSTFn2(fn) {
  return function(a, b) {
    return fn(a)(b)();
  };
};
  
export const mkSTFn3 = function mkSTFn3(fn) {
  return function(a, b, c) {
    return fn(a)(b)(c)();
  };
};
  
export const mkSTFn4 = function mkSTFn4(fn) {
  return function(a, b, c, d) {
    return fn(a)(b)(c)(d)();
  };
};
  
export const mkSTFn5 = function mkSTFn5(fn) {
  return function(a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e)();
  };
};
  
export const mkSTFn6 = function mkSTFn6(fn) {
  return function(a, b, c, d, e, f) {
    return fn(a)(b)(c)(d)(e)(f)();
  };
};
  
export const mkSTFn7 = function mkSTFn7(fn) {
  return function(a, b, c, d, e, f, g) {
    return fn(a)(b)(c)(d)(e)(f)(g)();
  };
};
  
export const mkSTFn8 = function mkSTFn8(fn) {
  return function(a, b, c, d, e, f, g, h) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)();
  };
};
  
export const mkSTFn9 = function mkSTFn9(fn) {
  return function(a, b, c, d, e, f, g, h, i) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)();
  };
};
  
export const mkSTFn10 = function mkSTFn10(fn) {
  return function(a, b, c, d, e, f, g, h, i, j) {
    return fn(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)();
  };
};
  
export const runSTFn1 = function runSTFn1(fn) {
  return function(a) {
    return function() {
      return fn(a);
    };
  };
};
  
export const runSTFn2 = function runSTFn2(fn) {
  return function(a) {
    return function(b) {
      return function() {
        return fn(a, b);
      };
    };
  };
};
  
export const runSTFn3 = function runSTFn3(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function() {
          return fn(a, b, c);
        };
      };
    };
  };
};
  
export const runSTFn4 = function runSTFn4(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function() {
            return fn(a, b, c, d);
          };
        };
      };
    };
  };
};
  
export const runSTFn5 = function runSTFn5(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function() {
              return fn(a, b, c, d, e);
            };
          };
        };
      };
    };
  };
};
  
export const runSTFn6 = function runSTFn6(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function() {
                return fn(a, b, c, d, e, f);
              };
            };
          };
        };
      };
    };
  };
};
  
export const runSTFn7 = function runSTFn7(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(g) {
                return function() {
                  return fn(a, b, c, d, e, f, g);
                };
              };
            };
          };
        };
      };
    };
  };
};
  
export const runSTFn8 = function runSTFn8(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(g) {
                return function(h) {
                  return function() {
                    return fn(a, b, c, d, e, f, g, h);
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
  
export const runSTFn9 = function runSTFn9(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(g) {
                return function(h) {
                  return function(i) {
                    return function() {
                      return fn(a, b, c, d, e, f, g, h, i);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
  
export const runSTFn10 = function runSTFn10(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(g) {
                return function(h) {
                  return function(i) {
                    return function(j) {
                      return function() {
                        return fn(a, b, c, d, e, f, g, h, i, j);
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};