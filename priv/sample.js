

const jObject = Object;
const jMap    = Map;


const fac = defmatch(
	clause([0], function(_0) {
		return 1;
	}), 
	clause([$_], function(N) {
		return N * fac(N - 1);
	}))

module.exports = {
  fac: fac
}

start();
