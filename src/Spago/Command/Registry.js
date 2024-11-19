import readlineSync from 'readline-sync';

export function question(query) {
  return readlineSync.question(query, {
    hideEchoBack: true // The typed text on screen is hidden by `*`
  });
}
