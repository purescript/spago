import mm from 'micromatch';
import * as fsWalk from '@nodelib/fs.walk';

export const testGlob = glob => mm.matcher(glob.include, { ignore: glob.ignore });

export const fsWalkImpl = Left => Right => respond => options => path => () => {
  const entryFilter = entry => options.entryFilter(entry)();
  const deepFilter = entry => options.deepFilter(entry)();
  fsWalk.walk(path, { entryFilter, deepFilter }, (error, entries) => {
    if (error !== null) return respond(Left(error))();
    return respond(Right(entries))();
  });
};

export const isFile = dirent => dirent.isFile();

export const direntToString = dirent => JSON.stringify(dirent);
