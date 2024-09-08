#include <iostream>
#include "PersistentDAG_2.h"

using namespace DAGNamespace;

struct Foo {
	template <unsigned char ancestor_count>
	using _a_integral = std::integral_constant<unsigned char, ancestor_count>;
	using _a_node_var_raw = std::variant<
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__non_updatable, _a_integral<0>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<1>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<2>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<3>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<4>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<5>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<6>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<7>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<8>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__ancestor_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<0>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<1>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<2>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<3>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<4>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<5>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<6>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<7>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<8>>*,
		PersistentDAG_2<Foo>::DAG_node<DAG_node_type__invariant_updatable, _a_integral<ANCESTOR_COUNT__EXCEED>>*>;

	int _i;
	std::string _s;
	_a_node_var_raw _node_var_raw;
	Foo(int i, const std::string& s) : _i(i), _s(s) {};
	bool inspect_invariant() const {
		return true;
	};
	void set_DAG_node(const _a_node_var_raw& node_var_raw) {
		_node_var_raw = node_var_raw;
	};
};

void f3() {
	std::array<_a_node_location, 1> ancestor_nodes_00 = { { _a_node_location(0, 0) } };
	std::array<_a_node_location, 2> ancestor_nodes_0001 = { { _a_node_location(0, 0), _a_node_location(0, 1) } };
	std::array<_a_node_location, 2> ancestor_nodes_0110 = { { _a_node_location(0, 1), _a_node_location(1, 0) } };
	std::array<_a_node_location, 4> ancestor_nodes_00011021 = { {
			_a_node_location(0, 0),
			_a_node_location(0, 1),
			_a_node_location(1, 0),
			_a_node_location(2, 1) } };
	auto node_location_40 = _a_node_location(4, 0);
	Foo foo5{ 5, std::string("5") };
	Foo foo6{ 6, std::string("6") };
	Foo foo7{ 7, std::string("7") };
	Foo foo8{ 8, std::string("8") };

	PersistentDAG_2<Foo> DAG0{};
	auto DAG1{ DAG0.emplace<DAG_node_type__non_updatable>(1, std::string("1")) };
	auto DAG2{ DAG1.emplace<DAG_node_type__invariant_updatable>(2, std::string("2")) };
	auto DAG3{ DAG2.emplace<DAG_node_type__ancestor_updatable, 1>(ancestor_nodes_00, 3, std::string("3")) };
	auto DAG4{ DAG3.emplace<DAG_node_type__invariant_updatable, 2>(ancestor_nodes_0001, 4, std::string("4")) };
	auto DAG5{ DAG4.insert<DAG_node_type__non_updatable>(foo5) };
	auto DAG6{ DAG5.insert<DAG_node_type__invariant_updatable>(foo6) };
	auto DAG7{ DAG6.insert<DAG_node_type__ancestor_updatable, 2>(ancestor_nodes_0110, foo7) };
	auto DAG8{ DAG7.insert<DAG_node_type__invariant_updatable, 4>(ancestor_nodes_00011021, foo8) };
	auto DAG9{ DAG8.erase<4>(node_location_40) };
	auto beg_{ DAG9.value().cbegin() };
	auto end_{ DAG9.value().cend() };
	for (auto it = beg_; it != end_; ++it) {
		std::cout << "i: " << it->_i << ", s: " << it->_s << std::endl;
	}
	int i = 2;
	return;
};

int main()
{
	f3();
	return 0;
}
