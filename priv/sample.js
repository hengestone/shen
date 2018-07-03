

const get_types = function() {
  return Object, Map, Array, String;
}


const start2 = pattern({
	'X,Y': function(X,Y) {
		const [Object,Map,_Array,_String] = get_types();
		const MyMap = Object.create(Map);
		const F = function(A) {
		return A.log('100');
	};
		F(console);
		return (pattern({
	'1': function() {
		return console.log([X,Y]);
	},
	'_': function() {
		return console.log('ok');
	}})
)(X);
	}});

const start = pattern({
	'': function() {
		h1({id: 'myproduct_id'});
		start2(1,3);
		const J = 5;
		const N = fac(J);
		return console.log('factorial ~p',[J,N]);
	}});

const fac = pattern({
	'0': function(_0) {
		return 1;
	},	'N': function(N) {
		return N * fac(N - 1);
	}});

module.exports = {
  start: start,
  start2: start2,
  fac: fac,
  macro: macro,
  main: main
}

start();
