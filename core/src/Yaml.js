import Yaml from 'yaml';

export const getImpl = (doc, path, keep) =>
  doc.get(path, keep);

export const addImpl = (doc, path, value) =>
  doc.add(path, value);

export const deleteImpl = (doc, path) =>
  doc.delete(path);

export const hasImpl = (doc, path) =>
  doc.has(path);

export const toSeqItems = (doc) =>
  Yaml.isSeq(doc) ? doc.items : [];

export const isMap = (doc) => Yaml.isMap(doc);

export function yamlParserImpl(fail, succ, s) {
  try {
    // TODO: we should only support JSON values in the parsing,
    // because we convert to JSON right after parsing.
    // But to parse YAML we need to have a separate AST I guess?
    return succ(Yaml.parse(s, { schema: 'core' }));
  }
  catch (e) {
    return fail(e.message);
  }
}

export function yamlDocParserImpl(fail, succ, s) {
  const doc = Yaml.parseDocument(s);
  if (doc.errors.length === 0) {
    return succ(doc);
  } else {
    // TODO: eh, we should return properly structured errors here
    return fail(JSON.stringify(doc.errors));
  }
}

export function toJsonImpl(doc) {
  return doc.toJSON();
}

export function toStringImpl(doc) {
  return doc.toString();
}

export function stringify(j) {
  return Yaml.stringify(j);
}

export function stringifyWithIndent(i) {
  return function (j) {
    return Yaml.stringify(j, null, i);
  };
}
