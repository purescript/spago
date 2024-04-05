import mm from 'micromatch';
import * as fsWalk from '@nodelib/fs.walk';

export const micromatch = options => patterns => mm.matcher(patterns, options);

export const fsWalkImpl = Left => Right => respond => options => path => () => {
  const entryFilter = entry => options.entryFilter(entry)();
  const deepFilter = entry => options.deepFilter(entry)();
  fsWalk.walk(path, { entryFilter, deepFilter }, (error, entries) => {
    if (error !== null) return respond(Left(error))();
    return respond(Right(entries))();
  });
};

export const isFile = dirent => dirent.isFile();

