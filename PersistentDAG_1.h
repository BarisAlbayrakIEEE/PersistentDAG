/*!
 * see the README file of the github repository below for an overview of this persistent DAG interface.
 * see PersistentDAG_2.h for the 2nd version of this interface.
 *
 * @author    <baris.albayrak.ieee@gmail.com>
 * @version   0.0.0
 * github:    <https://github.com/BarisAlbayrakIEEE/cpp.git>
 */

#ifndef _PersistentDAG_1_HeaderFile
#define _PersistentDAG_1_HeaderFile

#include <cstring>
#include <concepts>
#include <vector>
#include <stack>
#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <tuple>
#include <memory>
#include <limits.h>
#include <mutex>
#include <future>
#include "read_only_queue_wrapper.h"
#include "VectorTree.h"

namespace DAGNamespace {
	struct traversal_type_BFS;
	struct traversal_type_DFS;
	struct direction_type_ancestor;
	struct direction_type_descendant;

	enum class EDAG_node_types : unsigned char {
		non_updatable,
		self_updatable,
		ancestor_updatable,
		invariant_updatable
	};
	enum class EDAG_node_states : unsigned char {
		uptodate,
		invariant_fail,
		ancestor_fail,
		directed_cycle,
		deleted
	};

	/*!
	 * @brief erases the element with the input index
	 * by swapping with the last element
	 * followed by a pop_back
	 * only the index (i.e. iterator) of the last element is invalidated
	 * and must be corrected
	 */
	template <typename T>
	inline void erase__by_swap_and_pop__std_vector__nonconst(
		std::vector<T>& vec,
		std::size_t index_erase)
	{
		auto& element_erase{ vec[index_erase] };
		auto& element_last{ vec.back() };
		std::swap(element_erase, element_last);
		vec.pop_back();
	};

	/*!
	 * @brief erases the element with the input index
	 * by swapping with the last element
	 * followed by a pop_back
	 * only the index (i.e. iterator) of the last element is invalidated
	 * and must be corrected
	 */
	template <typename T>
	[[nodiscard]] inline auto erase__by_swap_and_pop__std_vector__const(
		const std::vector<T>& vec1,
		std::size_t index_erase)
		-> std::vector<T>
	{
		std::vector<T> vec2{ vec1 };
		auto& element_erase{ vec2[index_erase] };
		auto& element_last{ vec2.back() };
		std::swap(element_erase, element_last);
		vec2.pop_back();
		return vec2;
	};






	/*!
	 * @brief PersistentDAG_1 class
	 * @see the main documentation of this header file for the details
	 */
	template <typename T>
	class PersistentDAG_1 {
		template <typename DAG_type, typename traversal_type, typename direction_type>
		friend struct DAG_iterator_base;
		template <typename DAG_type, typename traversal_type, typename direction_type>
		friend class DAG_const_iterator;
		template <typename DAG_type, typename traversal_type, typename direction_type>
		friend class DAG_iterator;

		// Local aliases
		using _u_que_wrap = read_only_queue_wrapper<std::vector<std::size_t>>;




		static const std::size_t _head_index{ SIZE_MAX - 1 };
		static const std::size_t _tail_index{ SIZE_MAX };

		// Members
		mutable std::mutex _mutex;
		std::atomic<bool> _valid_state{ true };
		VectorTree<T> _datas;
		std::vector<EDAG_node_types> _DAG_node_types;
		std::vector<EDAG_node_states> _DAG_node_states{ EDAG_node_states::uptodate, EDAG_node_states::uptodate };
		std::vector<std::vector<std::size_t>> _indexs__ancestor;
		std::vector<std::vector<std::size_t>> _indexs__descendant;
		std::vector<std::size_t> _indexs__head__descendants;
		std::vector<std::size_t> _indexs__tail__ancestors;
		std::vector<std::size_t> _indexs__deleted;
		std::unordered_map<std::size_t, std::vector<std::size_t>> _indexs__cycled;




		/*!
		 * base class for STL style iterators
		 * 
		 * @see the main documentation of this header file for the details
		 * 
		 * CAUTION:
		 *     the constructor, update__indexs__pool and set_next_index may throw.
		 *     hence, the increment and decrement operators of the iterators may throw.
		 *     the strong exception safety must be considered while using the iterators
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		struct DAG_iterator_base
		{
			friend class PersistentDAG_1<T>;

			// Local aliases
			using _u_DAG_raw = DAG_type const*;
			using _u_pool_type = std::conditional_t<
				std::is_same_v<traversal_type, traversal_type_BFS>,
				std::queue<_u_que_wrap>,
				std::stack<_u_que_wrap>>;

			// Members
			std::vector<bool> _indexs__visited;
			_u_DAG_raw _DAG_raw{};
			std::size_t _index;
			_u_pool_type _indexs__pool;

			// this member provides skiping the children
			// (i.e. descendants if the iteration is in descendant direction).
			// the iteration neglects/skips the path stating from the current node (i.e. _index).
			// do not reset the value to false
			// after skipping a path when required.
			// otherwise the iteration will skip all paths
			// 
			// see the documentation of PersistentDAG_1::propogate_DAG_node_state
			// for an example usage
			bool _skip_remaining{};

			DAG_iterator_base() noexcept = default;
			template <typename direction_type2 = direction_type>
				requires (std::is_same_v<direction_type2, direction_type_ancestor>)
			explicit DAG_iterator_base(_u_DAG_raw DAG_raw)
				:
				_indexs__visited(std::vector<bool>(DAG_raw->_indexs__ancestor.size())),
				_DAG_raw(DAG_raw)
			{
				auto& indexs__tail__ancestors{ DAG_raw->_indexs__tail__ancestors };
				if (indexs__tail__ancestors.empty()) {
					_index = _DAG_raw->get_end_index<direction_type2>();
				}
				else {
					_indexs__pool.emplace(&indexs__tail__ancestors);
					_index = indexs__tail__ancestors[0];
					_indexs__visited[_index] = true;
				}
			};
			template <typename direction_type2 = direction_type>
				requires (std::is_same_v<direction_type2, direction_type_descendant>)
			explicit DAG_iterator_base(_u_DAG_raw DAG_raw)
				:
				_indexs__visited(std::vector<bool>(DAG_raw->_indexs__ancestor.size())),
				_DAG_raw(DAG_raw)
			{
				auto& indexs__head_descendants{ DAG_raw->_indexs__head__descendants };
				if (indexs__head_descendants.empty()) {
					_index = _DAG_raw->get_end_index<direction_type2>();
				}
				else {
					_indexs__pool.emplace(&indexs__head_descendants);
					_index = indexs__head_descendants[0];
					_indexs__visited[_index] = true;
				}
			};
			DAG_iterator_base(_u_DAG_raw DAG_raw, std::size_t index) noexcept
				:
				_indexs__visited(std::vector<bool>(DAG_raw->_indexs__ancestor.size())),
				_DAG_raw(DAG_raw),
				_index{ index }
			{
				_indexs__visited[index] = true;
			};
			DAG_iterator_base(_u_DAG_raw DAG_raw, std::size_t index, bool) noexcept
				:
				_DAG_raw(DAG_raw),
				_index{ index } {};

			/*!
			 * @brief updates the iterator pool
			 * base class
			 * @exceptsafe may throw bad_alloc
			 */
			template <typename direction_type2 = direction_type>
			inline void update__indexs__pool() {
				auto& indexs__ancestor = _DAG_raw->_indexs__ancestor[_index];
				if (!indexs__ancestor.empty()) {
					_indexs__pool.emplace(&indexs__ancestor);
				}
			};
			/*!
			 * @brief updates the iterator pool
			 * specialization for descendant direction
			 * @exceptsafe may throw bad_alloc
			 */
			template <>
			inline void update__indexs__pool<direction_type_descendant>() {
				auto& indexs__descendant = _DAG_raw->_indexs__descendant[_index];
				if (!indexs__descendant.empty()) {
					_indexs__pool.emplace(&indexs__descendant);
				}
			};

			[[nodiscard]] _u_que_wrap& get_next_from_pool(std::stack<_u_que_wrap>& container) {
				return container.top();
			};
			[[nodiscard]] _u_que_wrap& get_next_from_pool(std::queue<_u_que_wrap>& container) {
				return container.front();
			};

			/*!
			 * @brief this method sets the next index
			 * no recursion in order to reduce the stack usage
			 * @exceptsafe may throw bad_alloc
			 */
			void set_next_index()
			{
				if (_DAG_raw->_DAG_node_states[_index] != EDAG_node_states::directed_cycle &&
					!_skip_remaining)
				{
					this->update__indexs__pool<direction_type>();
				}
				while (!_indexs__pool.empty()) {
					while (!_indexs__pool.empty()) {
						if (
							const auto& indexs__current{ get_next_from_pool(_indexs__pool) };
							!indexs__current.empty())
						{
							break;
						}
						_indexs__pool.pop();
					}
					if (_indexs__pool.empty()) {
						_index = _DAG_raw->get_end_index<direction_type>();
						return;
					}

					auto& indexs__current{ get_next_from_pool(_indexs__pool) };
					_index = indexs__current.front();
					if (!_indexs__visited[_index]) {
						_indexs__visited[_index] = true;
						return;
					}
				}
				_index = _DAG_raw->get_end_index<direction_type>();
			};
		};





	public:

		/*!
		 * @brief The STL style const iterator class
		 * 
		 * template parameters:
		 *     traversal_type == traversal_type_BFS || traversal_type == traversal_type_DFS
		 *     direction_type == direction_type_ancestor || direction_type == direction_type_descendant
		 * 
		 * @see DAG_iterator_base for the details
		 * 
		 * Base template for:
		 *     direction_type == direction_type_ancestor
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		class DAG_const_iterator
			: public DAG_iterator_base<DAG_type, traversal_type, direction_type>
		{
			friend class PersistentDAG_1<T>;

			// Local aliases
			using _u_base = DAG_iterator_base<DAG_type, traversal_type, direction_type>;

		public:

			using _u_base::_u_base;

			// STL aliases
			using iterator_category = std::bidirectional_iterator_tag;
			using value_type = typename DAG_type::value_type;
			using difference_type = typename DAG_type::difference_type;
			using pointer = typename DAG_type::const_pointer;
			using reference = const value_type&;

			[[nodiscard]] inline reference operator*() const noexcept {
				return this->_DAG_raw->_datas[this->_index];
			};

			[[nodiscard]] inline pointer operator->() const noexcept {
				return &this->_DAG_raw->_datas[this->_index];
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			inline DAG_const_iterator& operator++() noexcept {
				_u_base::set_next_index();
				return *this;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			[[nodiscard]] inline DAG_const_iterator operator++(int) noexcept {
				DAG_const_iterator _temp{ *this };
				++*this;
				return _temp;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			[[nodiscard]] inline DAG_const_iterator operator+(const difference_type offset) noexcept {
				DAG_const_iterator _temp{ *this };
				_temp += offset;
				return _temp;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			DAG_const_iterator& operator+=(const difference_type offset) noexcept {
				auto index_end{ this->_DAG_raw->get_end_index<direction_type>() };
				for (std::ptrdiff_t i = 0; i < offset; ++i) {
					if (this->_index == index_end) { return *this; }
					operator++();
				}
				return *this;
			};

			[[nodiscard]] inline bool operator==(const DAG_const_iterator& rhs) const noexcept {
				return this->_DAG_raw == rhs._DAG_raw && this->_index == rhs._index;
			};

			[[nodiscard]] inline bool operator!=(const DAG_const_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};
		};






	private:

		/*!
		 * @brief The STL style iterator class
		 * Follows std::vector::iterator approach which bases std::vector::const_iterator
		 * 
		 * @see the documentation of DAG_const_iterator for the details
		 * 
		 * Notice that the non-const iterator is private
		 * to keep the persistency of DAG.
		 */
		template <typename DAG_type, typename traversal_type, typename direction_type>
		class DAG_iterator
			: public DAG_const_iterator<DAG_type, traversal_type, direction_type>
		{
			friend class PersistentDAG_1<T>;

		public:

			// STL aliases
			using iterator_category = std::bidirectional_iterator_tag;
			using value_type = typename DAG_type::value_type;
			using difference_type = typename DAG_type::difference_type;
			using pointer = typename DAG_type::pointer;
			using reference = value_type&;

			// Local aliases
			using _u_base = DAG_const_iterator<DAG_type, traversal_type, direction_type>;

			// Constructors
			using _u_base::_u_base;

			[[nodiscard]] inline reference operator*() const noexcept {
				return const_cast<reference>(_u_base::operator*());
			};

			[[nodiscard]] inline pointer operator->() const noexcept {
				return const_cast<pointer>(_u_base::operator->());
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			inline DAG_iterator& operator++() noexcept {
				_u_base::operator++();
				return *this;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			[[nodiscard]] inline DAG_iterator operator++(int) noexcept {
				DAG_iterator _temp{ *this };
				_u_base::operator++();
				return _temp;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			[[nodiscard]] inline DAG_iterator operator+(const difference_type offset) const noexcept {
				DAG_iterator _temp{ *this };
				_u_base::operator+=(offset);
				return _temp;
			};

			/*!
			 * @exceptsafe may throw bad_alloc
			 * @see the documentation of DAG_iterator_base
			 */
			inline DAG_iterator& operator+=(const difference_type offset) noexcept {
				_u_base::operator+=(offset);
				return *this;
			};

			[[nodiscard]] inline bool operator==(const DAG_iterator& rhs) const noexcept {
				return this->_DAG_raw == rhs._DAG_raw && this->_index == rhs._index;
			};

			[[nodiscard]] inline bool operator!=(const DAG_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};
		};





	public:

		// STL aliases
		using value_type = T;
		using allocator_type = std::allocator<T>;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using iterator = DAG_iterator<PersistentDAG_1<T>, traversal_type_DFS, direction_type_ancestor>;
		using const_iterator = DAG_const_iterator<PersistentDAG_1<T>, traversal_type_DFS, direction_type_ancestor>;
		using reference = T&;
		using const_reference = const T&;
		using size_type = std::size_t;
		using difference_type = std::ptrdiff_t;





	private:

		// Local aliases
		template <typename traversal_type, typename direction_type>
		using iterator_type = DAG_iterator<PersistentDAG_1<T>, traversal_type, direction_type>;
		template <typename traversal_type, typename direction_type>
		using const_iterator_type = DAG_const_iterator<PersistentDAG_1<T>, traversal_type, direction_type>;
		using iterator_BFS_ancestor = iterator_type<traversal_type_BFS, direction_type_ancestor>;
		using iterator_BFS_descendant = iterator_type<traversal_type_BFS, direction_type_descendant>;
		using iterator_DFS_ancestor = iterator_type<traversal_type_DFS, direction_type_ancestor>;
		using iterator_DFS_descendant = iterator_type<traversal_type_DFS, direction_type_descendant>;
		using const_iterator_BFS_ancestor = const_iterator_type<traversal_type_BFS, direction_type_ancestor>;
		using const_iterator_BFS_descendant = const_iterator_type<traversal_type_BFS, direction_type_descendant>;
		using const_iterator_DFS_ancestor = const_iterator_type<traversal_type_DFS, direction_type_ancestor>;
		using const_iterator_DFS_descendant = const_iterator_type<traversal_type_DFS, direction_type_descendant>;

		/*!
		 * @brief non-const iterator - begin
		 * internal usage only
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_descendant> begin() {
			return iterator_type<traversal_type, direction_type_descendant>(this);
		};
		/*!
		 * @brief non-const iterator - end
		 * internal usage only
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_descendant> end() {
			return iterator_type<traversal_type, direction_type_descendant>(this, _tail_index, true);
		};
		/*!
		 * @brief non-const iterator - rbegin
		 * internal usage only
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_ancestor> rbegin() {
			return iterator_type<traversal_type, direction_type_ancestor>(this);
		};
		/*!
		 * @brief non-const iterator - rend
		 * internal usage only
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline iterator_type<traversal_type, direction_type_ancestor> rend() {
			return iterator_type<traversal_type, direction_type_ancestor>(this, _head_index, true);
		};





		/*!
		 * @brief gets the end index of the iteration
		 * Base template for
		 *     direction_type == direction_type_ancestor
		 */
		template <typename direction_type>
		[[nodiscard]] inline std::size_t get_end_index() const noexcept
		{
			return _head_index;
		};

		/*!
		 * @brief gets the end index of the iteration
		 * Specialization for
		 *     direction_type == direction_type_descendant
		 */
		template <>
		[[nodiscard]] inline std::size_t get_end_index<direction_type_descendant>() const noexcept {
			return _tail_index;
		};




		/*!
		 * @brief a helper function to copy all members accept for the datas into the new DAG
		 * @exceptsafe may throw bad_alloc
		 */
		void copy_members_accept_for_datas(PersistentDAG_1<T>& new_DAG) const
		{
			new_DAG._DAG_node_types = _DAG_node_types;
			new_DAG._indexs__ancestor = _indexs__ancestor;
			new_DAG._indexs__descendant = _indexs__descendant;
			new_DAG._indexs__head__descendants = _indexs__head__descendants;
			new_DAG._indexs__tail__ancestors = _indexs__tail__ancestors;
			new_DAG._indexs__deleted = _indexs__deleted;
			new_DAG._indexs__cycled = _indexs__cycled;

			std::scoped_lock l(_mutex);
			new_DAG._DAG_node_states = _DAG_node_states;

			auto valid_state{ _valid_state.load(std::memory_order_acquire) };
			new_DAG._valid_state.store(valid_state, std::memory_order_relaxed);
		};


		


		/*!
		 * @brief DAG topological comparison - less than
		 * notice that two nodes do not have to be related.
		 * in other words, Not(a < b) does not imply a > b.
		 * 
		 * by definition a < b if a can be reached
		 * by a traversal in ancestor direction starting from b.
		 * 
		 * use DFS traversal as the memory usage is less for DFS comparingly
		 * and the runtime performance is same as the DAG is unweighted.
		 * 
		 * use a traversal in descendant direction
		 * as the user would mostly work close to the tail
		 * 
		 * @exceptsafe the iterator may throw
		 */
		[[nodiscard]] bool inspect_topologically_less(
			std::size_t index__ancestor,
			std::size_t index__descendant) const
		{
			auto it{ const_iterator_DFS_descendant(this, index__ancestor) };
			auto ite{ cend<traversal_type_DFS>() };
			auto itf{ const_iterator_DFS_descendant(this, index__descendant) };
			for (; it != ite; ++it) {
				if (it == itf) { return true; }
			}
			return false;
		};

		/*!
		 * @brief DAG topological comparison - greater than
		 * see the documentation of inspect_topologically_less for the details
		 * 
		 * use DFS traversal as the memory usage is less for DFS comparingly
		 * and the runtime performance is same as the DAG is unweighted.
		 * 
		 * use a traversal in descendant direction
		 * as the user would mostly work close to the tail
		 * 
		 * @exceptsafe the iterator may throw
		 */
		[[nodiscard]] bool inspect_topologically_greater(
			std::size_t index__descendant,
			std::size_t index__ancestor) const
		{
			auto it{ const_iterator_DFS_descendant(this, index__ancestor) };
			auto ite{ cend<traversal_type_DFS>() };
			auto itf{ const_iterator_DFS_descendant(this, index__descendant) };
			for (; it != ite; ++it) {
				if (it == itf) { return true; }
			}
			return false;
		};

		/*!
		 * @brief inspect if a directed cycle is formed by the input nodes.
		 * 
		 * start the iteration from the input descendant
		 * and perform the iteration in the descendant direction
		 * in order to reduce the iteration length.
		 * because, if there is no cycle,
		 * the iteration will terminate at the end (i.e. tail for descendant traversal)
		 * and mostly the user works close to the tail node.
		 * 
		 * @exceptsafe the iterator may throw
		 */
		[[nodiscard]] bool inspect_directed_cycle(
			std::size_t index__ancestor,
			std::size_t index__descendant) const
		{
			return inspect_topologically_less(index__descendant, index__ancestor);
		};





		/*!
		 * @brief helper function to update the state of a node
		 * when the ancestors has changed
		 *     - for this DAG
		 * 
		 * strong exception safety by constness
		 */
		void update_DAG_node_state__this(
			std::vector<EDAG_node_states>& DAG_node_states_,
			std::unordered_map<std::size_t, std::vector<std::size_t>>& indexs__cycled_,
			std::size_t index) const
		{
			// check if no update issue
			auto DAG_node_type__this{ _DAG_node_types[index] };
			if (DAG_node_type__this == EDAG_node_types::non_updatable ||
				DAG_node_type__this == EDAG_node_types::self_updatable) { return; }

			// check if previously deleted
			auto DAG_node_state__this{ DAG_node_states_[index] };
			if (DAG_node_state__this == EDAG_node_states::deleted) { return; }

			// inspect cycle
			const auto& indexs__ancestor{ _indexs__ancestor[index] };
			if (DAG_node_state__this == EDAG_node_states::directed_cycle) {
				if (const auto& indexs__cycled{ indexs__cycled_[index] };
					std::find_first_of(
						indexs__cycled.cbegin(), indexs__cycled.cend(),
						indexs__ancestor.cbegin(), indexs__ancestor.cend()) !=
					indexs__cycled.cend())
				{
					return;
				}
				DAG_node_state__this = EDAG_node_states::uptodate;
				indexs__cycled_.erase(index);
			}

			// inspect state of ancestors
			bool all_uptodate{ true };
			for (auto index__ancestor : indexs__ancestor) {
				auto DAG_node_state__ancestor{ DAG_node_states_[index__ancestor] };
				if (static_cast<unsigned char>(DAG_node_state__ancestor) >
					static_cast<unsigned char>(DAG_node_state__this))
				{
					DAG_node_state__this = DAG_node_state__ancestor;
				}
				if (DAG_node_state__ancestor != EDAG_node_states::uptodate)
				{
					all_uptodate = false;
				}
			}
			if (all_uptodate)
			{
				DAG_node_state__this = EDAG_node_states::uptodate;
			}

			// inspect invariant of T
			if (DAG_node_type__this == EDAG_node_types::invariant_updatable &&
				DAG_node_state__this == EDAG_node_states::uptodate)
			{
				DAG_node_state__this = _datas[index].inspect_invariant();
			}

			// set the DAG_node_state
			DAG_node_states_[index] = DAG_node_state__this;
		};

		/*!
		 * @brief function to update the state of a node
		 * when the ancestors has changed
		 *     - for the copy of the DAG
		 * 
		 * @exceptsafe may throw as propogate_DAG_node_state may throw
		 */
		void inline update_DAG_node_state__new(std::size_t index)
		{
			auto DAG_node_state__initial{ _DAG_node_states[index] };
			update_DAG_node_state__this(
				_DAG_node_states,
				_indexs__cycled,
				index);
			if (_DAG_node_states[index] != EDAG_node_states::uptodate) {
				_valid_state = false;
			}
			if (_DAG_node_states[index] != DAG_node_state__initial) {
				propogate_DAG_node_state(index);
			}
		};

		/*!
		 * @brief updates the state of a node
		 * when the state of an ancestor has changed
		 * 
		 * strong exception safety
		 * as no memory allocation, no user type, no recursion, etc.
		 */
		void update_DAG_node_state__propogate(
			std::size_t index,
			EDAG_node_states DAG_node_state__propogated) noexcept
		{
			auto DAG_node_state__initial{ _DAG_node_states[index] };
			if (DAG_node_state__initial == DAG_node_state__propogated) { return; }

			// check if no update issue
			auto DAG_node_type__this{ _DAG_node_types[index] };
			if (DAG_node_type__this == EDAG_node_types::non_updatable ||
				DAG_node_type__this == EDAG_node_types::self_updatable) { return; }

			// check if previously deleted
			if (DAG_node_state__initial == EDAG_node_states::deleted) { return; }

			// check if previously cycled
			if (DAG_node_state__initial == EDAG_node_states::directed_cycle) { return; }

			// inspect ancestors
			auto DAG_node_state__new{ DAG_node_state__initial };
			if (DAG_node_state__propogated == EDAG_node_states::uptodate) {
				const auto& indexs__ancestor{ _indexs__ancestor[index] };
				bool all_uptodate{ true };
				for (auto index__ancestor : indexs__ancestor) {
					auto DAG_node_state__ancestor{ _DAG_node_states[index__ancestor] };
					if (static_cast<unsigned char>(DAG_node_state__ancestor) >
						static_cast<unsigned char>(DAG_node_state__new))
					{
						DAG_node_state__new = DAG_node_state__ancestor;
					}
					if (DAG_node_state__ancestor != EDAG_node_states::uptodate)
					{
						all_uptodate = false;
					}
				}
				if (all_uptodate)
				{
					DAG_node_state__new = EDAG_node_states::uptodate;
				}
			}
			else {
				const auto& indexs__ancestor{ _indexs__ancestor[index] };
				for (auto index__ancestor : indexs__ancestor) {
					auto DAG_node_state__ancestor{ _DAG_node_states[index__ancestor] };
					if (static_cast<unsigned char>(DAG_node_state__ancestor) >
						static_cast<unsigned char>(DAG_node_state__new))
					{
						DAG_node_state__new = DAG_node_state__ancestor;
					}
				}
			}

			// inspect invariant of T
			if (DAG_node_type__this == EDAG_node_types::invariant_updatable &&
				DAG_node_state__initial != EDAG_node_states::uptodate &&
				DAG_node_state__new == EDAG_node_states::uptodate)
			{
				DAG_node_state__new = _datas[index].inspect_invariant();
			}

			// set the DAG_node_state
			_DAG_node_states[index] = DAG_node_state__new;
		};

		/*!
		 * @brief propogates the change in the state of a node through the descendants
		 * 
		 * uses BFS traversal as it propogates faster
		 * 
		 * uses _skip_remaining member of the iterator
		 * to skip a path in the graph if required:
		 *     consider the state of a node A has changed
		 *     and requires an iteration to propogate the state change.
		 *     consider node A has two descendants: nodes B and C.
		 *     consider node B has two descendants: nodes D and E.
		 *     assume the state of node B does not change
		 *     when the iteration reaches node B.
		 *     there is no need to continue the iteration from node B
		 *     to inspect nodes D and E
		 *     as the state of node B remains the same.
		 *     so the iteration will skip the descendants of the node B
		 *     and continue with node C.
		 * 
		 * @exceptsafe the iterator may throw bad_alloc
		 */
		void inline propogate_DAG_node_state(std::size_t index)
		{
			auto it{ iterator_BFS_descendant(this, index) };
			auto ite{ end<traversal_type_BFS>() };
			auto DAG_node_state__this{ _DAG_node_states[index] };
			for (; it != ite; ++it) {
				auto DAG_node_state__initial{ _DAG_node_states[it._index] };
				update_DAG_node_state__propogate(it._index, DAG_node_state__this);
				if (_DAG_node_states[it._index] == DAG_node_state__initial) {
					it._skip_remaining = true;
				}
				else {
					it._skip_remaining = false;
				}
			}
		};





		/*!
		 * @brief helper function to replace the ancestors of a node
		 * strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto replace_ancestors_helper(
			std::size_t index,
			const T& data_new,
			const std::vector<std::size_t>& indexs__ancestor__new) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// get the current ancestors of the input node
			auto& indexs__ancestor__old{ _indexs__ancestor[index] };

			// set the data
			new_DAG._datas = _datas.set_data(index, data_new);

			// copy other members
			copy_members_accept_for_datas(new_DAG);

			// modify the descendants of the current ancestors
			for (auto index__ancestor : indexs__ancestor__old) {
				auto& indexs__descendant__new{ new_DAG._indexs__descendant[index__ancestor] };
				auto it = std::find(
					indexs__descendant__new.begin(),
					indexs__descendant__new.end(),
					index);
				indexs__descendant__new.erase(it);
			}

			// modify the descendants of the new ancestors
			for (auto index__ancestor : indexs__ancestor__new) {
				auto& indexs__descendant__new{ new_DAG._indexs__descendant[index__ancestor] };
				indexs__descendant__new.push_back(index);
			}

			// replace the ancestors of the input node
			new_DAG._indexs__ancestor[index] = indexs__ancestor__new;

			// inspect head/tail nodes
			for (auto index_ : indexs__ancestor__old) {
				auto& indexs__descendant_{ _indexs__descendant[index_] };;
				if (indexs__descendant_.empty()) {
					new_DAG._indexs__tail__ancestors.push_back(index_);
				}
			}
			for (auto index_ : indexs__ancestor__new) {
				auto it = std::find(
					new_DAG._indexs__tail__ancestors.begin(),
					new_DAG._indexs__tail__ancestors.end(),
					index_);
				if (it != new_DAG._indexs__tail__ancestors.end()) {
					new_DAG._indexs__tail__ancestors.erase(it);
				}
			}

			// update the DAG_node_state
			new_DAG.update_DAG_node_state__new(index);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief erase element - mutating
		 * used by erase_deleted_nodes
		 * see the documentation of erase_deleted_nodes for the details
		 * 
		 * @exceptsafe may throw
		 */
		void erase_mutate(std::size_t index_erase)
		{
			// get the ancestors of the input node
			auto index_last{ _indexs__ancestor.size() - 1 };
			const auto& indexs__ancestor__erase{ _indexs__ancestor[index_erase] };
			const auto& indexs__ancestor__last{ _indexs__ancestor[index_last] };
			const auto& indexs__descendant__last{ _indexs__descendant[index_last] };

			// erase the index from the head/tail descendants/ancestors
			if (
				auto it1 = std::find(
					_indexs__tail__ancestors.begin(),
					_indexs__tail__ancestors.end(),
					index_erase); 
				it1 != _indexs__tail__ancestors.end())
			{
				_indexs__tail__ancestors.erase(it1);
			}
			if (
				auto it2 = std::find(
					_indexs__head__descendants.begin(),
					_indexs__head__descendants.end(),
					index_erase); 
				it2 != _indexs__head__descendants.end())
			{
				_indexs__head__descendants.erase(it2);
			}

			// erase the node by using erase__by_swap_and_pop
			// this function invalidates the index of only the last element
			// which can be corrected easily in contant time
			_datas = _datas.erase__by_swap_and_pop(index_erase);
			_DAG_node_types = erase__by_swap_and_pop__std_vector__nonconst(
				_DAG_node_types, index_erase);
			_DAG_node_states = erase__by_swap_and_pop__std_vector__nonconst(
				_DAG_node_states, index_erase);
			_indexs__ancestor = erase__by_swap_and_pop__std_vector__nonconst(
				_indexs__ancestor, index_erase);
			_indexs__descendant = erase__by_swap_and_pop__std_vector__nonconst(
				_indexs__descendant, index_erase);

			// the cycled indices
			if (!_indexs__cycled.contains(index_last)) {
				_indexs__cycled.erase(index_erase);
			}
			else {
				auto pair_ = _indexs__cycled.extract(index_last);
				pair_.key() = index_erase;
				_indexs__cycled.insert(std::move(pair_));
			}

			// remove the erased index from the descendants of the ancestors of the erased index
			for (
				auto it1 = indexs__ancestor__erase.cbegin();
				it1 != indexs__ancestor__erase.cend();
				it1++)
			{
				auto& indexs__descendant{ _indexs__descendant[*it1] };
				auto it2 = std::find(
					indexs__descendant.begin(),
					indexs__descendant.end(),
					index_erase);
				indexs__descendant.erase(it2);
			}

			// cannot have descendants to be erased
			;

			// correct the ancestors/descendants of the
			// descendants/ancestors of the invalidated last index
			for (auto index__ancestor : indexs__ancestor__last) {
				auto& indexs__descendant{ _indexs__descendant[index__ancestor] };
				auto it = std::find(
					indexs__descendant.begin(),
					indexs__descendant.end(),
					index_last);
				if (it != indexs__descendant.end()) { *it = index_erase; }
			}
			for (auto index__descendant : indexs__descendant__last) {
				auto& indexs__ancestor{ _indexs__ancestor[index__descendant] };
				auto it = std::find(
					indexs__ancestor.begin(),
					indexs__ancestor.end(),
					index_last);
				if (it != indexs__ancestor.end()) { *it = index_erase; }
			}

			// set the DAG node index of the contained type
			// for the invalidated last index
			_datas[index_erase].set_DAG_node(index_erase);
		};





	public:

		/*!
		 * @brief default constructor
		 */
		PersistentDAG_1() noexcept = default;

		/*!
		 * @brief copy constructor
		 * 
		 * strong exception safety is satisfied by using temporaries.
		 * another method would be reserving the vectors
		 * before copying with std::copy.
		 * but that would require the contained type T
		 * to have a non-throwing copy constructor.
		 * 
		 * @see the main documentation of this header file
		 * for a discussion on the performance of the copy constructor
		 */
		PersistentDAG_1(const PersistentDAG_1& rhs)
			:
			_datas(rhs._datas),
			_DAG_node_types(rhs._DAG_node_types),
			_indexs__ancestor(rhs._indexs__ancestor),
			_indexs__descendant(rhs._indexs__descendant),
			_indexs__head_descendants(rhs._indexs__head__descendants),
			_indexs__tail__ancestors(rhs._indexs__tail__ancestors),
			_indexs__deleted(rhs._indexs__deleted),
			_indexs__cycled(rhs._indexs__cycled)
		{
			// copy _DAG_node_states if the background thread has joined
			std::scoped_lock l(rhs._mutex);
			_DAG_node_states = rhs._DAG_node_states;

			auto valid_state{ rhs._valid_state.load(std::memory_order_acquire) };
			_valid_state.store(valid_state, std::memory_order_relaxed);
		};

		/*!
		 * @brief copy assignment
		 * strong exception safety by copy-and-swap idiom
		 */
		PersistentDAG_1& operator=(PersistentDAG_1 rhs) {
			swap(*this, rhs);
			return *this;
		};

		/*!
		 * @brief friend swap function for the copy-and-swap idiom
		 */
		friend inline void swap(PersistentDAG_1& lhs, PersistentDAG_1& rhs) noexcept {
			using std::swap;
			swap(lhs._indexs__cycled, rhs._indexs__cycled);
			swap(lhs._valid_state, rhs._valid_state);
			swap(lhs._datas, rhs._datas);
			swap(lhs._DAG_node_types, rhs._DAG_node_types);
			swap(lhs._DAG_node_states, rhs._DAG_node_states);
			swap(lhs._indexs__ancestor, rhs._indexs__ancestor);
			swap(lhs._indexs__descendant, rhs._indexs__descendant);
			swap(lhs._indexs__head__descendants, rhs._indexs__head__descendants);
			swap(lhs._indexs__tail__ancestors, rhs._indexs__tail__ancestors);
			swap(lhs._indexs__deleted, rhs._indexs__deleted);
		};

		/*!
		 * @brief move constructor
		 * 
		 * CAUTION:
		 * not default:
		 *     std::unordered_map does not have noexcept move constructor.
		 *     but it has a noexcept move assignment
		 */
		PersistentDAG_1(PersistentDAG_1&& rhs) noexcept {
			move_helper(std::move(rhs));
		};

		/*!
		 * @brief move assignment
		 * 
		 * CAUTION:
		 * not default:
		 *     node state data requires a lock
		 */
		PersistentDAG_1& operator=(PersistentDAG_1&& rhs) noexcept {
			move_helper(std::move(rhs));
			return *this;
		};

		/*!
		 * @brief move helper
		 * all members including std::unordered_map has noexcept move assignment
		 */
		void move_helper(PersistentDAG_1&& rhs) noexcept {
			_indexs__cycled = std::move(rhs._indexs__cycled);
			_datas = std::move(rhs._datas);
			_DAG_node_types = std::move(rhs._DAG_node_types);
			_indexs__ancestor = std::move(rhs._indexs__ancestor);
			_indexs__descendant = std::move(rhs._indexs__descendant);
			_indexs__head__descendants = std::move(rhs._indexs__head__descendants);
			_indexs__tail__ancestors = std::move(rhs._indexs__tail__ancestors);
			_indexs__deleted = std::move(rhs._indexs__deleted);

			// move _DAG_node_states if the background thread has joined
			std::scoped_lock l(rhs._mutex);
			_DAG_node_states = std::move(rhs._DAG_node_states);

			auto valid_state{ rhs._valid_state.load(std::memory_order_acquire) };
			_valid_state.store(valid_state, std::memory_order_relaxed);
		};

		~PersistentDAG_1() = default;

		[[nodiscard]] inline bool operator==(const PersistentDAG_1& rhs) const noexcept {
			return _datas == rhs._datas;
		};

		/*!
		 * @brief const iterators - cbegin
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_descendant> cbegin() const {
			return const_iterator_type<traversal_type, direction_type_descendant>(this);
		};
		/*!
		 * @brief const iterators - cend
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_descendant> cend() const {
			return const_iterator_type<traversal_type, direction_type_descendant>(this, _tail_index, true);
		};
		/*!
		 * @brief const iterators - crbegin
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_ancestor> crbegin() const {
			return const_iterator_type<traversal_type, direction_type_ancestor>(this);
		};
		/*!
		 * @brief const iterators - crend
		 * @exceptsafe not noexcept as the iterator constructor may throw bad_alloc
		 */
		template <typename traversal_type = traversal_type_BFS>
		[[nodiscard]] inline const_iterator_type<traversal_type, direction_type_ancestor> crend() const {
			return const_iterator_type<traversal_type, direction_type_ancestor>(this, _head_index, true);
		};





		/*!
		 * @brief insert element
		 * Overload 1: without ancestors
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		template <typename U = T>
		[[nodiscard]] auto insert(
			EDAG_node_types DAG_node_type,
			EDAG_node_states DAG_node_state,
			U&& data) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// push the data
			new_DAG._datas = _datas.push_back(std::forward<U>(data));
			auto index__new{ new_DAG._datas.size() - 1 };

			// copy other members
			copy_members_accept_for_datas(new_DAG);

			// push the type
			new_DAG._DAG_node_types.push_back(DAG_node_type);

			// push the ancestors and descendants
			new_DAG._indexs__ancestor.emplace_back();
			new_DAG._indexs__descendant.emplace_back();

			// push the DAG_node_state
			new_DAG._DAG_node_states.push_back(DAG_node_state);

			// modify the descendant indices of the head
			new_DAG._indexs__head__descendants.push_back(index__new);

			// modify the ancestor indices of the tail
			new_DAG._indexs__tail__ancestors.push_back(index__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief insert element
		 * Overload 2: with ancestors
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		template <
			typename Container = std::vector<std::size_t>,
			typename U = T>
		[[nodiscard]] auto insert(
			Container&& indexs__ancestor,
			EDAG_node_types DAG_node_type,
			EDAG_node_states DAG_node_state,
			U&& data) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// push the data
			new_DAG._datas = _datas.push_back(std::forward<U>(data));
			auto index__new{ new_DAG._datas.size() - 1 };

			// copy other members
			copy_members_accept_for_datas(new_DAG);

			// push the type
			new_DAG._DAG_node_types.push_back(DAG_node_type);

			// push the ancestors and descendants
			new_DAG._indexs__ancestor.push_back(std::forward<Container>(indexs__ancestor));
			new_DAG._indexs__descendant.emplace_back();

			// push the DAG_node_state
			new_DAG._DAG_node_states.push_back(DAG_node_state);

			// modify the descendant indices of the input ancestors
			const auto& indexs__ancestor__recover{ new_DAG._indexs__ancestor.back() };
			for (auto index__ancestor : indexs__ancestor__recover) {
				auto& indexs__descendant{ new_DAG._indexs__descendant[index__ancestor] };
				indexs__descendant.push_back(index__new);
			}

			// modify the ancestor indices of the tail
			new_DAG._indexs__tail__ancestors.push_back(index__new);
			for (auto index__ancestor : indexs__ancestor__recover) {
				auto it = std::find(
					new_DAG._indexs__tail__ancestors.begin(),
					new_DAG._indexs__tail__ancestors.end(),
					index__ancestor);
				if (it != new_DAG._indexs__tail__ancestors.end()) {
					new_DAG._indexs__tail__ancestors.erase(it);
				}
			}

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};




		/*!
		 * @brief emplace element
		 * Overload 1: without ancestors
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		template <typename... Ts>
		[[nodiscard]] auto emplace(
			EDAG_node_types DAG_node_type,
			EDAG_node_states DAG_node_state,
			Ts&&... args) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// emplace the data
			new_DAG._datas = _datas.emplace_back(std::forward<Ts>(args)...);
			auto index__new{ new_DAG._datas.size() - 1 };

			// copy other members
			copy_members_accept_for_datas(new_DAG);

			// push the type
			new_DAG._DAG_node_types.push_back(DAG_node_type);

			// push the ancestors and descendants
			new_DAG._indexs__ancestor.emplace_back();
			new_DAG._indexs__descendant.emplace_back();

			// push the DAG_node_state
			new_DAG._DAG_node_states.push_back(DAG_node_state);

			// modify the descendant indices of the head
			new_DAG._indexs__head__descendants.push_back(index__new);

			// modify the ancestor indices of the tail
			new_DAG._indexs__tail__ancestors.push_back(index__new);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief emplace element
		 * Overload 2: with ancestors
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		template <
			typename Container = std::vector<std::size_t>,
			typename... Ts>
		[[nodiscard]] auto emplace(
			Container&& indexs__ancestor,
			EDAG_node_types DAG_node_type,
			EDAG_node_states DAG_node_state,
			Ts&&... args) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// emplace the data
			new_DAG._datas = _datas.emplace_back(std::forward<Ts>(args)...);
			auto index__new{ new_DAG._datas.size() - 1 };

			// copy other members
			copy_members_accept_for_datas(new_DAG);

			// push the type
			new_DAG._DAG_node_types.push_back(DAG_node_type);

			// push the ancestors and descendants
			new_DAG._indexs__ancestor.push_back(std::forward<Container>(indexs__ancestor));
			new_DAG._indexs__descendant.emplace_back();

			// push the DAG_node_state
			new_DAG._DAG_node_states.push_back(DAG_node_state);

			// modify the descendant indices of the input ancestors
			const auto& indexs__ancestor__recover{ new_DAG._indexs__ancestor.back() };
			for (auto index__ancestor : indexs__ancestor__recover) {
				auto& indexs__descendant{ new_DAG._indexs__descendant[index__ancestor] };
				indexs__descendant.push_back(index__new);
			}

			// modify the ancestor indices of the tail
			new_DAG._indexs__tail__ancestors.push_back(index__new);
			for (auto index__ancestor : indexs__ancestor__recover) {
				auto it = std::find(
					new_DAG._indexs__tail__ancestors.begin(),
					new_DAG._indexs__tail__ancestors.end(),
					index__ancestor);
				if (it != new_DAG._indexs__tail__ancestors.end()) {
					new_DAG._indexs__tail__ancestors.erase(it);
				}
			}

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};




		/*!
		 * @brief erases an element
		 * 
		 * terminates if the input has descendants
		 * inform the user in such a case and call erase_with_descendants
		 * if the user insists on erasing the element
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto erase(std::size_t index_erase) const
			-> std::optional<PersistentDAG_1<T>>
		{
			if (!_indexs__descendant[index_erase].empty()) {
				return {};
			}

			// create a new DAG
			PersistentDAG_1<T> new_DAG{};

			// get the ancestors of the input node
			auto index_last{ _indexs__ancestor.size() - 1 };
			const auto& indexs__ancestor__erase{ _indexs__ancestor[index_erase] };
			const auto& indexs__ancestor__last{ _indexs__ancestor[index_last] };
			const auto& indexs__descendant__last{ _indexs__descendant[index_last] };

			// copy head/tail descendants/ancestors
			new_DAG._indexs__tail__ancestors = _indexs__tail__ancestors;
			new_DAG._indexs__head__descendants = _indexs__head__descendants;

			// erase the index from the head/tail descendants/ancestors
			if (
				auto it1 = std::find(
					new_DAG._indexs__tail__ancestors.begin(),
					new_DAG._indexs__tail__ancestors.end(),
					index_erase); 
				it1 != new_DAG._indexs__tail__ancestors.end())
			{
				new_DAG._indexs__tail__ancestors.erase(it1);
			}
			if (
				auto it2 = std::find(
					new_DAG._indexs__head__descendants.begin(),
					new_DAG._indexs__head__descendants.end(),
					index_erase); 
				it2 != new_DAG._indexs__head__descendants.end())
			{
				new_DAG._indexs__head__descendants.erase(it2);
			}

			// erase the node by using erase__by_swap_and_pop
			// this function invalidates the index of only the last element
			// which can be corrected easily in constant time
			new_DAG._datas = _datas.erase__by_swap_and_pop(index_erase);
			new_DAG._DAG_node_types = erase__by_swap_and_pop__std_vector__const(
				_DAG_node_types, index_erase);
			new_DAG._DAG_node_states = erase__by_swap_and_pop__std_vector__const(
				_DAG_node_states, index_erase);
			new_DAG._indexs__ancestor = erase__by_swap_and_pop__std_vector__const(
				_indexs__ancestor, index_erase);
			new_DAG._indexs__descendant = erase__by_swap_and_pop__std_vector__const(
				_indexs__descendant, index_erase);

			// the cycled indices
			new_DAG._indexs__cycled = _indexs__cycled;
			if (!new_DAG._indexs__cycled.contains(index_last)) {
				new_DAG._indexs__cycled.erase(index_erase);
			}
			else {
				auto pair_ = new_DAG._indexs__cycled.extract(index_last);
				pair_.key() = index_erase;
				new_DAG._indexs__cycled.insert(std::move(pair_));
			}

			// remove the erased index from the descendants of the ancestors of the erased index
			for (
				auto it1 = indexs__ancestor__erase.cbegin();
				it1 != indexs__ancestor__erase.cend();
				it1++)
			{
				auto& indexs__descendant{ new_DAG._indexs__descendant[*it1] };
				auto it2 = std::find(
					indexs__descendant.begin(),
					indexs__descendant.end(),
					index_erase);
				indexs__descendant.erase(it2);
			}

			// correct the ancestors/descendants of the
			// descendants/ancestors of the invalidated last index
			for (auto index__ancestor : indexs__ancestor__last) {
				auto& indexs__descendant{ new_DAG._indexs__descendant[index__ancestor] };
				auto it = std::find(
					indexs__descendant.begin(),
					indexs__descendant.end(),
					index_last);
				if (it != indexs__descendant.end()) { *it = index_erase; }
			}
			for (auto index__descendant : indexs__descendant__last) {
				auto& indexs__ancestor{ new_DAG._indexs__ancestor[index__descendant] };
				auto it = std::find(
					indexs__ancestor.begin(),
					indexs__ancestor.end(),
					index_last);
				if (it != indexs__ancestor.end()) { *it = index_erase; }
			}

			// set the DAG node index of the contained type
			// for the invalidated last index
			new_DAG._datas[index_erase].set_DAG_node(index_erase);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};

		/*!
		 * @brief updates the state as deleted for an index with descendant nodes
		 * 
		 * erase function above terminates
		 * if the input has descendants.
		 * this function must be executed
		 * if the user insists on erasing the element.
		 * hence, erase function must be executed before this function.
		 * 
		 * this function sets the state of the node as deleted
		 * instead of erasing the element
		 * and updates the state of the descendant nodes.
		 * later the user may recover the states.
		 * 
		 * @see the main documentation of this header file for the details.
		 *
		 * @exceptsafe may throw due to the copy constructor of the DAG
		 * strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto erase_with_descendants(std::size_t index_erase) const
			-> PersistentDAG_1<T>
		{
			// create a new DAG
			PersistentDAG_1<T> new_DAG{ *this };

			// set the state of the node as deleted
			new_DAG._DAG_node_states[index_erase] = EDAG_node_states::deleted;

			// propogate the state change
			new_DAG.propogate_DAG_node_state(index_erase);

			// execute the background thread
			auto valid_state{ new_DAG._valid_state.load(std::memory_order_acquire) };
			if (!valid_state) {
				auto fut = std::async(&PersistentDAG_1::update_DAG_state, &new_DAG); // allow deferred execution
			}
			return new_DAG; // ensure NRVO due to single return
		};




		/*!
		 * @brief replace the ancestors of a node
		 * 
		 * terminates if the result is a directed cycle.
		 * inform the user in such a case
		 * and call replace_ancestors_forced
		 * if the user insists on replacing the ancestors
		 * 
		 * @exceptsafe may throw std::logic_error if a directed cycle is detected
		 * @exceptsafe may throw due to the copy constructor of the DAG and the iterator
		 * strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto replace_ancestors(
			std::size_t index,
			const T& data_new,
			const std::vector<std::size_t>& indexs__ancestor__new) const
			-> PersistentDAG_1<T>
		{
			using _u_out = std::variant<PersistentDAG_1<T>, std::vector<std::size_t>>;

			// inspect directed cycle
			for (auto index__ancestor : indexs__ancestor__new) {
				if (inspect_directed_cycle(index__ancestor, index)) {
					return {};
				}
			}
			return std::make_optional<PersistentDAG_1<T>>( // RVO is guaranteed by the standard
				replace_ancestors_helper(
					index,
					data_new,
					indexs__ancestor__new));
		};

		/*!
		 * @brief replaces the ancestors of a node
		 * 
		 * should be executed when the new ancestors contain node(s)
		 * which are already descendant(s) of the node.
		 * in other words, a directed cycle is formed
		 * resulting with a DAG having invalid state.
		 * 
		 * replace_ancestors function above terminates
		 * if a directed cycle is formed.
		 * this function must be executed
		 * if the user insists on replacing the ancestors.
		 * hence, replace_ancestors function must be executed before this function.
		 * 
		 * @exceptsafe may throw due to the copy constructor of the DAG and the iterator
		 * strong exception safety is a direct result of the persistency.
		 */
		[[nodiscard]] auto replace_ancestors_forced(
			std::size_t index,
			const T& data_new,
			const std::vector<std::size_t>& indexs__ancestor__new) const
			-> PersistentDAG_1<T>
		{
			std::vector<std::size_t> indexs__cycled;
			for (auto index__ancestor : indexs__ancestor__new) {
				if (inspect_directed_cycle(index__ancestor, index)) {
					indexs__cycled.push_back(index__ancestor);
				}
			}
			auto new_DAG{ replace_ancestors_helper(index, data_new, indexs__ancestor__new) };
			new_DAG._indexs__cycled[index] = std::move(indexs__cycled);

			return new_DAG; // ensure NRVO due to single return
		};





		/*!
		 * @brief inspects and updates the state of the DAG
		 * executed by the background thread
		 * 
		 * use BFS traversal:
		 *     BFS is a level-by-level traversal
		 *     which ensures the state of the ancestors are examined
		 *     when performing in the descendant direction.
		 *     an additional descendant iteration is required for each node
		 *     if a descendant DFS traversal is used
		 *     which makes the linear runtime quadratic.
		 * 
		 * notice that the function terminates
		 * if DAG is in a valid state (i.e. all nodes are uptodate)
		 * 
		 * one of the only two mutating functions together with erase_deleted_nodes
		 * which are prepared for the background threads.
		 * this function traverses through the whole DAG.
		 * hence, the runtime complexity is O(kN).
		 * however, the function holds only the lock on _mutex__state
		 * which allows copy of the members accept for _DAG_node_states
		 * in the copy constructor.
		 * hence, the main thread can do essential work
		 * while the background thread executes this function.
		 * the other background process, erase_deleted_nodes,
		 * executes faster if the deleted node count is small.
		 * 
		 * @exceptsafe may throw due to the copy constructor of the DAG and the iterator
		 * but satisfies strong exception safety
		 * as the initial data is backed up
		 */
		void update_DAG_state()
		{
			// backup the state data as the iterator may throw
			std::vector<EDAG_node_states> DAG_node_states;
			std::unordered_map<std::size_t, std::vector<std::size_t>> indexs__cycled;
			{
				std::scoped_lock l(_mutex);
				DAG_node_states = _DAG_node_states;
				indexs__cycled = _indexs__cycled;
			}

			// update the states
			auto it{ begin() };
			auto ite{ end() };
			bool valid_state{ true };
			for (; it != ite; ++it) {
				update_DAG_node_state__this(
					DAG_node_states,
					indexs__cycled,
					it._index);
				if (DAG_node_states[it._index] != EDAG_node_states::uptodate) {
					valid_state = false;
				}
			}
			{
				std::scoped_lock l(_mutex);
				_DAG_node_states = DAG_node_states;
				_indexs__cycled = indexs__cycled;

				_valid_state.store(valid_state, std::memory_order_relaxed);
			}
		};

		/*!
		 * @brief erases the nodes with deleted state and without descendant nodes
		 * 
		 * DAG allows deleting nodes without deleting its descendants.
		 * this results with an ill-formed DAG.
		 * In such a case, the node is not deleted but
		 * the state is set to deleted.
		 * see the main documentation of this header file for the details.
		 * 
		 * later, the user may recover the state of the ill-formed DAG
		 * by changing the ancestors of the descendants of the deleted node or
		 * by deleting the descendants of the deleted node.
		 * 
		 * this function erases the deleted nodes
		 * for which the DAG state is recovered later by the user
		 * (i.e. the nodes with deleted state and without descendant nodes).
		 * 
		 * use DFS traversal:
		 *     as a BFS traversal consumes more space
		 * 
		 * one of the only two mutating functions together with update_DAG_state
		 * which are prepared for the background threads.
		 * this function does not have a traversal through the whole DAG
		 * as the DAG stores the nodes with deleted state.
		 * hence, the runtime for this function is short
		 * unless the number of the nodes with deleted state is large
		 * 
		 * @exceptsafe erase_mutate may throw
		 * but satisfies strong exception safety
		 * as the initial data is backed up
		 */
		void erase_deleted_nodes()
		{
			if (_indexs__deleted.empty()) return;

			// backup the state data as the iterator may throw
			PersistentDAG_1<T> temp;
			{
				std::scoped_lock l(_mutex);
				temp = *this;
			}

			// erase the deleted nodes
			try {
				for (auto index__deleted : _indexs__deleted) {
					if (_indexs__descendant[index__deleted].empty()) {
						erase_mutate(index__deleted);
					}
				}
			}
			catch (...) {
				*this = std::move(temp);
			}
			if (_indexs__deleted.empty()) {
				update_DAG_state();
			};
		};
	};
};

#endif
