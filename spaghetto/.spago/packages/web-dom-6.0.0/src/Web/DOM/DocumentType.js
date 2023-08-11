var getProp = function (name) {
  return function (doctype) {
    return doctype[name];
  };
};

export const name = getProp("name");
export const publicId = getProp("publicId");
export const systemId = getProp("systemId");
