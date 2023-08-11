import ssh2 from "ssh2";

export function parseKeyImpl(left, right, buffer, passphrase) {
  const parsed = ssh2.utils.parseKey(buffer, passphrase);
  if (parsed && parsed.type && parsed.comment) {
    return right(parsed);
  } else {
    return left(parsed);
  }
}

export function signImpl(parsedKey, data) {
  return parsedKey.sign(data).toString("hex");
}

export function verifyImpl(parsedKey, data, signature) {
  return parsedKey.verify(data, Buffer.from(signature, "hex"));
}

export function keyTypeImpl(parsedKey) {
  return parsedKey.type;
}

export function isPrivateKeyImpl(parsedKey) {
  return parsedKey.isPrivateKey();
}

export function equalsImpl(a, b) {
  return a.equals(b);
}
