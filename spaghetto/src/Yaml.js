import Yaml from 'yaml';

export function _yamlParser(fail, succ, s) {
  try {
    // TODO:
    // We should only support JSON values in the parsing, because we convert to JSON right after parsing.
    // But to parse YAML we need to have a separate AST I guess?
    return succ(Yaml.parse(s, { schema: 'core' }));
  }
  catch (e) {
    return fail(e.message);
  }
}

export function stringify(j) {
  return Yaml.stringify(j);
}

export function stringifyWithIndent(i) {
  return function (j) {
    return Yaml.stringify(j, null, i);
  };
}