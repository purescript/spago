import { setGracefulCleanup, dirSync } from "tmp";

setGracefulCleanup();

export const mkTmpDirImpl = () => {
  const tmpobj = dirSync();
  return tmpobj.name;
};
