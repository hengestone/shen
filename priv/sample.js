

const jObject = Object;
const jMap    = Map;


const start2 = defmatch(
	clause([$_, $_], function(X, Y) {
		const _MyMap = jObject.create(jMap);
		const F = function(A) {
		return A.log('100');
	};
		F(console);
		return (defmatch(
	clause([1], function() {
		return console.log([X, Y]);
	}),
	clause([y], function() {
		return console.log('ok');
	}))
)(X);
	}))

const start = defmatch(
	clause([], function() {
		h1({id: 'myproduct_id'});
		start2(1, 3);
		const J = 5;
		const N = fac(J);
		const [_A, _B] = start2(J, N);
		return console.log('factorial ~p', [J, N]);
	}))

const fac = defmatch(
	clause([0], function(_0) {
		return 1;
	}), 	clause([$_], function(N) {
		return N * fac(N - 1);
	}))

module.exports = {
  start: start,
  start2: start2,
  fac: fac,
  macro: macro,
  main: main
}

start();
