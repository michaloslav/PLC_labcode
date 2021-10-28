const automatonExample = {
  start: 'a',
  trans: [
    ['a', 0, 'a'],
    ['a', 1, 'b'],
    ['b', 0, 'b'],
    ['b', 1, 'c'],
    ['c', 0, 'c'],
    ['c', 1, 'c'],
  ],
  final: ['c']
}

const nextStates = (automaton, state, letter) => automaton.trans
  .filter(([tState, tLetter]) => state === tState && letter === tLetter)
  .map(([tState, tLetter, tNextState]) => [tLetter, tNextState]);

function autoTrSys(automaton, state) {
  const startState = state || automaton.start;
  const statesAfter1 = automaton.trans
    .filter(([state]) => state === startState)
    .map(el => el[2]);
  return statesAfter1.map(el => autoTrSys(automaton, el)).flat(Infinity);
}

function member(automaton, word) {
  const letters = word.split('');
  let states = [automaton.start];
  letters.forEach(letter => {
    states = states.map(state => nextStates(automaton, state, letter)).flat();
  });
  return states.any(state => automaton.final.includes(state));
}