import readlineSync from 'readline-sync';

export function questionPassword(query) {
  return readlineSync.question(query, {
    hideEchoBack: true // The typed text on screen is hidden by `*`
  });
}
