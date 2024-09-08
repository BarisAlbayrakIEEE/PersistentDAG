/*!
 * VectorTree interface
 * 
 * the basic dynamic array data structure (std::vector)
 * is in most cases the most efficient data structure
 * as stated by Stroustrup.
 * 
 * additional to the fundamental issues,
 * the most important feature of the standard vector is
 * the contiguous memory allocation
 * which enhances the cache efficiency of algorithms.
 * the power of the standard vector by the contiguous memory allocation
 * is a very important requirement
 * especially in performance critical applications.
 * 
 * on the other hand, the standard vector has two disadvantages:
 *     1. Copy operation is linear (O(N))
 *        although the contiguous allocation provides some efficiency
 *        (e.g. bitwise copy with memcopy if the contained element is trivially copyable)
 *     2. Iterators may become invalid due to the reallocation/shrinking (e.g. push_back)
 * 
 * the basic data structure in functional programming (FP)
 * is the linked list as it addresses solutions to the above two issues.
 * however, the linked list is cumbersome and
 * and a linked list has a very weak performance
 * as the iteration is based on the pointer indirection.
 * 
 * hence, in FP, we need a persistent data structure like a linked list
 * which, as well, provides a contiguous memory allocation like a vector.
 * 
 * Ivan Cukic, in his famous book about FP (Functional Programming in C++)
 * describes Bitmapped Vector Trie (BVT) like data structure
 * invented by Rick Hickey for the Clojure language.
 * Rick Hickey has also been inspired by Phil Bagwell's famous paper, Ideal Hash Trees.
 * Phill Bagwell's trie structure is Hash Array Mapped Trie (HAMT).
 * thus, in summary, the data structure in this header, VectorTree,
 * is a persistent replacement for the standard vector and
 * HAMT is a persistent replacement for
 * the associative data structures (std::unordered_set).
 * 
 * note that VectorTree is not a trie but a tree.
 * 
 * the VectorTree implemented in this header aggrees with
 * the definitions, invariants, algorithms and runtime/space complexities
 * defined by Ivan Cukic in Functional Programming in C++.
 * 
 * the implementation splits the vector
 * into small buffers (usually 32 elements)
 * which are allocated in a tree structure.
 * the buffers are allocated in the leaf nodes.
 * the composite nodes shares the ownership of the leafs.
 * 
 * the efficiency of the VectorTree data structure
 * converges to the efficiency of the standard vector
 * for the operations applied at the end
 * (e.g. push_back, emplace_back or pop_back).
 * however, unlike a linked list,
 * the actions applied not at the end are expensive
 * even more expensive than a standard vector.
 * 
 * in this interface,
 * a work-around is applied for the erase function
 * based on the swap and pop idiom.
 * however, this solution invalidates
 * the pointer to the last element,
 * which is an obvious confliction for a persistent data structure.
 * additionally, the order is not preserved as the last item is
 * swapped with the item to be deleted.
 * however, this is neither a standard library (STL) nor
 * a public open source library (e.g. boost).
 * hence, as an in-house data structure,
 * this confliction can be acceptable.
 * 
 * the following approach is followed for the operations at the end:
 *     1. find the path from the root node to
 *        the active leaf node (i.e. the active path)
 *     2. copy (not clone!) all the nodes in the active path
 *        copying the nodes means
 *        incrementing the shared reference counts on the nodes.
 *        cloning the nodes means duplicating the nodes.
 *        only VectorTree copy constructor performs clone on the nodes.
 *     3. modify the active leaf node for the requested operation
 *        together with the copied composite nodes
 *     4. create a new VectorTree
 *        which increments the shared references for the non-modified nodes
 *        and replaces the nodes in the active path with the modified nodes.
 * 
 * the above algorithm provides logN time complexity
 * which yields to a constant time as the base of the log is large (32 usually).
 * 
 * the algorithm may require a few modifications
 * if the active leaf node cannot handle the requested operation.
 * for example, the next leaf node must be determined
 * if the active leaf node is full and the request is a push.
 * considering these exceptional cases,
 * the worst case time complexity becomes logkN
 * which again yields to O(1).
 * 
 * obviously, all operations listed in the VectorTree interface are const-qualified
 * as its a functionally persistent data structure.
 * each operation returns a new VectorTree
 * keeping the persistent history of the data.
 * 
 * VectorTree has its own STL style random access iterators (const and non-const).
 * the non-const iterator is private and used internally.
 * STL style for_each algorithm is implemented as a member function
 * which clones (not copy!) the whole data structure and
 * applies the input function on each element
 * using the internal non-const iterator
 * 
 * 
 * 
 * \author    <baris.albayrak.ieee@gmail.com>
 * \version   0.0.a
 * @see       GeometryApplication.h FOR THE MAIN DOCUMENTATION OF THIS PROJECT
 * github:    <https://github.com/BarisAlbayrakIEEE/cpp.git>
 */

#ifndef _VectorTree_HeaderFile
#define _VectorTree_HeaderFile

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <optional>
#include <algorithm>
#include <numeric>
#include <stdexcept>
#include <utility>
#include <iterator>
#include <cmath>
#include <assert.h>

namespace DAGNamespace {
	struct VectorTree_node_type_leaf;
	struct VectorTree_node_type_composite;

	static const unsigned char DEFAULT_BUFFER = 32;
	static const unsigned char MAX_VectorTree_HEIGHT = 8;

	/*!
	 * @brief VectorTree class
	 * 
	 * the composite and leaf nodes and the iterators are implemented as inner classes.
	 * A static typing (templated class hyerarchy) is utilized
	 * together with std::variant
	 * instead of the dynamic polymorphism.
	 * 
	 * the iterators are almost the same as std::vector::iterator
	 * with a few modifications to keep track of the leaf buffers.
	 * 
	 * @see the main documentation of this header file for the details
	 */
	template <typename T, unsigned char BufferSize = DEFAULT_BUFFER, class Allocator = std::allocator<T>>
	class VectorTree {

		/*!
		 * @brief VectorTree_node class
		 * 
		 * base template for
		 *     VectorTree_node_type == VectorTree_node_type_leaf
		 * 
		 * VectorTree is a tree structure formed by composite and leaf nodes.
		 * a leaf node stores the data in a fixed size buffer (BufferSize).
		 */
		template <typename VectorTree_node_type>
		class VectorTree_node {
			friend class VectorTree<T, BufferSize>;
			friend class VectorTree_node<VectorTree_node_type_composite>;

			// local aliases
			using _u_leaf = VectorTree_node<VectorTree_node_type>;
			using _u_childs = std::vector<T, Allocator>;

			// members
			_u_childs _childs{};

		public:

			/*!
			 * @brief default constructor
			 * 
			 * reserves the size of the children vector
			 * in order to prevent std::vector reallocation
			 * for the safety of the pointers to the elements contained
			 */
			VectorTree_node() {
				_childs.reserve(BufferSize);
			};

			/*!
			 * @brief the big 5
			 * apply copy-and-swap idiom for the assignment operator
			 */
			VectorTree_node(const VectorTree_node& rhs) = default;
			VectorTree_node& operator=(VectorTree_node lhs) {
				swap(*this, lhs);
				return *this;
			}
			VectorTree_node(VectorTree_node&& rhs) noexcept = default;
			VectorTree_node& operator=(VectorTree_node&& rhs) noexcept = default;
			~VectorTree_node() = default;

			/*!
			 * @brief friend swap function
			 */
			friend inline void swap(_u_leaf& lhs, _u_leaf& rhs) noexcept {
				using std::swap;
				swap(lhs._childs, rhs._childs);
			};
		};

		/*!
		 * @brief VectorTree_node class
		 * 
		 * specialization for:
		 *     VectorTree_node_type == VectorTree_node_type_composite
		 * 
		 * VectorTree is a tree structure formed by composite and leaf nodes
		 * composite nodes may store other composites or leafs.
		 * 
		 * persistent VectorTree is based on the shared data
		 * in order to minimize the amount of the data to be copied.
		 * hence, the composite nodes have shared ownership on the children nodes.
		 */
		template <>
		class VectorTree_node<VectorTree_node_type_composite> {
			friend class VectorTree<T, BufferSize>;

			// local aliases
			using _u_composite = VectorTree_node<VectorTree_node_type_composite>;
			using _u_leaf = VectorTree_node<VectorTree_node_type_leaf>;
			using _u_node_sh_var = std::variant<std::shared_ptr<_u_composite>, std::shared_ptr<_u_leaf>>;
			using _u_childs = std::vector<_u_node_sh_var>;

			// members
			_u_childs _childs{};

			/*!
			 * @brief clone the node performing a deep copy
			 * instead of incrementing the reference counts
			 * 
			 * persistent VectorTree is based on the shared data
			 * in order to minimize the amount of the data to be copied.
			 * hence, the copy constructor of VectorTree_node
			 * does not perform a deep copy
			 * but increments the shared reference count.
			 * however, VectorTree copy constructor requires a deep copy
			 * 
			 * @throws clones composites recursively
			 * which may result with stack overflow or bad_alloc exceptions
			 * 
			 * @exceptsafe strong exception safety as non-mutating
			 */
			[[nodiscard]] auto clone() const -> std::shared_ptr<_u_composite>
			{
				_u_childs childs;
				if (!_childs.empty()) {
					if (std::holds_alternative<std::shared_ptr<_u_composite>>(_childs[0])) {
						for (auto& child : _childs) {
							auto composite_sh{ std::get<std::shared_ptr<_u_composite>>(child) };
							childs.emplace_back(composite_sh->clone());
						}
					}
					else {
						for (auto& child : _childs) {
							auto leaf_sh{ std::get<std::shared_ptr<_u_leaf>>(child) };
							childs.emplace_back(std::make_shared<_u_leaf>(*leaf_sh));
						}
					}
				}
				return std::make_shared<_u_composite>(childs);
			};

		public:

			VectorTree_node() { _childs.reserve(BufferSize); };
			explicit VectorTree_node(const _u_childs& childs) : _childs{childs} {};
			explicit VectorTree_node(unsigned char height) {
				assert(height > 0);

				_childs.reserve(BufferSize);
				if (height == 1) {
					for (auto i = 0; i < BufferSize; ++i) {
						_childs.emplace_back(std::make_shared<_u_leaf>());
					}
				}
				else {
					for (auto i = 0; i < BufferSize; ++i) {
						_childs.emplace_back(std::make_shared<_u_composite>(height - 1));
					}
				}
			};

			/*!
			 * @brief the big 5
			 * apply copy-and-swap idiom for the assignment operator
			 */
			VectorTree_node(const VectorTree_node& rhs) = default;
			VectorTree_node& operator=(VectorTree_node lhs) {
				swap(*this, lhs);
				return *this;
			}
			VectorTree_node(VectorTree_node&& rhs) noexcept = default;
			VectorTree_node& operator=(VectorTree_node&& rhs) noexcept = default;
			~VectorTree_node() = default;

			/*!
			 * @brief friend swap function
			 */
			friend inline void swap(_u_composite& lhs, _u_composite& rhs) noexcept {
				using std::swap;
				swap(lhs._childs, rhs._childs);
			};
		};

		/*!
		 * @brief the STL style const iterator class
		 * stores two additional data
		 * comparing with std::vector::const_iterator:
		 *     _leaf_node: current active leaf node:
		 *                 in order to increase iteration performance
		 *     _index: the index of the current element of the container:
		 *                 in order to realize the end iterator
		 */
		template <typename VectorTreeType>
		class VectorTree_const_iterator {
		public:

			// STL aliases
			using iterator_category = std::random_access_iterator_tag;
			using value_type = typename VectorTreeType::value_type;
			using difference_type = typename VectorTreeType::difference_type;
			using pointer = typename VectorTreeType::const_pointer;
			using reference = const value_type&;

			// local aliases
			using _u_container = VectorTreeType const*;
			using _u_leaf = VectorTree_node<VectorTree_node_type_leaf>*;

			// members
			_u_container _container{};
			_u_leaf _leaf_node{};
			pointer _pointer{};
			std::size_t _index{};

			VectorTree_const_iterator() noexcept = default;
			explicit VectorTree_const_iterator(_u_container container)
				:
				_container(container),
				_leaf_node{ container->get_leaf_node(container->get_path_to_leaf_node(0)) },
				_pointer{ &_leaf_node->_childs[0] } {};
			explicit VectorTree_const_iterator(_u_container container, _u_leaf leaf_node, pointer ptr) noexcept
				: _container(container), _leaf_node(leaf_node), _pointer{ ptr } {};

			[[nodiscard]] inline reference operator*() const noexcept {
				assert(_index < _container->size());
				return *_pointer;
			};

			[[nodiscard]] inline pointer operator->() const noexcept {
				return _pointer;
			};

			inline VectorTree_const_iterator& operator++() noexcept {
				this->operator+=(1);
				return *this;
			};

			[[nodiscard]] inline VectorTree_const_iterator operator++(int) noexcept {
				VectorTree_const_iterator iterator{ *this };
				++*this;
				return iterator;
			};

			[[nodiscard]] inline VectorTree_const_iterator operator+(const difference_type offset) noexcept {
				VectorTree_const_iterator iterator{ *this };
				iterator += offset;
				return iterator;
			};

			VectorTree_const_iterator& operator+=(const difference_type offset) noexcept {
				assert(_index + offset <= _container->size());

				if (_index + offset == _container->size()) {
					_index = _container->size();
					_pointer = _container->one_past_the_last_element_const();
				}
				else {
					_index += offset;
					if (auto it{ _leaf_node->_childs.cbegin() }; std::distance(&*it, _pointer + offset) < BufferSize - 1) {
						_pointer += offset;
					}
					else {
						_pointer = &_container->operator[](_index);
					}
				}
				return *this;
			};

			inline VectorTree_const_iterator& operator--() noexcept {
				this->operator-=(1);
				return *this;
			};

			[[nodiscard]] inline VectorTree_const_iterator operator--(int) noexcept {
				VectorTree_const_iterator iterator{ *this };
				--*this;
				return iterator;
			};

			[[nodiscard]] inline VectorTree_const_iterator operator-(const difference_type offset) noexcept {
				VectorTree_const_iterator iterator{ *this };
				iterator -= offset;
				return iterator;
			};

			inline VectorTree_const_iterator& operator-=(const difference_type offset) noexcept {
				assert(_index >= offset);

				_index -= offset;
				if (auto it{ _leaf_node->_childs.cbegin() }; std::distance(&*it, _pointer - offset) >= 0) {
					_pointer -= offset;
				}
				else {
					_pointer = _container->operator[](_index);
				}
				return *this;
			};

			[[nodiscard]] inline reference operator[](const difference_type offset) const noexcept {
				return *(*this + offset);
			};

			[[nodiscard]] inline bool operator==(const VectorTree_const_iterator& rhs) const noexcept {
				return _pointer == rhs._pointer;
			};

			[[nodiscard]] inline bool operator!=(const VectorTree_const_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};

			[[nodiscard]] inline bool operator<(const VectorTree_const_iterator& rhs) const noexcept {
				return _index < rhs._index;
			};

			[[nodiscard]] inline bool operator<=(const VectorTree_const_iterator& rhs) const noexcept {
				return _index <= rhs._index;
			};

			[[nodiscard]] inline bool operator>(const VectorTree_const_iterator& rhs) const noexcept {
				return _index > rhs._index;
			};

			[[nodiscard]] inline bool operator>=(const VectorTree_const_iterator& rhs) const noexcept {
				return _index >= rhs._index;
			};
		};

		/*!
		 * @brief the STL style iterator class
		 * follows std::vector::iterator approach which bases std::vector::const_iterator
		 */
		template <typename VectorTreeType>
		class VectorTree_iterator : public VectorTree_const_iterator<VectorTreeType> {

		public:

			// STL aliases
			using iterator_category = std::random_access_iterator_tag;
			using value_type = typename VectorTreeType::value_type;
			using difference_type = typename VectorTreeType::difference_type;
			using pointer = typename VectorTreeType::pointer;
			using reference = value_type&;

			// local aliases
			using _u_base = VectorTree_const_iterator<VectorTreeType>;
			using _u_container = VectorTreeType*;
			using _u_base::_u_base;

			[[nodiscard]] inline reference operator*() const noexcept {
				return const_cast<reference>(_u_base::operator*());
			}

			[[nodiscard]] inline pointer operator->() const noexcept {
				return this->_pointer;
			};

			inline VectorTree_iterator& operator++() noexcept {
				_u_base::operator++();
				return *this;
			};

			[[nodiscard]] inline VectorTree_iterator operator++(int) noexcept {
				VectorTree_iterator iterator = *this;
				_u_base::operator++();
				return iterator;
			};

			inline VectorTree_iterator& operator--() noexcept {
				_u_base::operator--();
				return *this;
			};

			[[nodiscard]] inline VectorTree_iterator operator--(int) noexcept {
				VectorTree_iterator iterator = *this;
				_u_base::operator--();
				return iterator;
			};

			inline VectorTree_iterator& operator+=(const difference_type offset) noexcept {
				_u_base::operator+=(offset);
				return *this;
			};

			[[nodiscard]] inline VectorTree_iterator operator+(const difference_type offset) const noexcept {
				VectorTree_iterator iterator = *this;
				iterator += offset;
				return iterator;
			};

			[[nodiscard]] friend inline VectorTree_iterator operator+(const difference_type offset, VectorTree_iterator rhs) noexcept {
				rhs += offset;
				return rhs;
			};

			inline VectorTree_iterator& operator-=(const difference_type offset) noexcept {
				_u_base::operator-=(offset);
				return *this;
			};

			using _u_base::operator-;

			[[nodiscard]] inline VectorTree_iterator operator-(const difference_type offset) const noexcept {
				VectorTree_iterator iterator = *this;
				iterator -= offset;
				return iterator;
			};

			[[nodiscard]] inline reference operator[](const difference_type offset) const noexcept {
				return *(*this + offset);
			};

			[[nodiscard]] inline bool operator==(const VectorTree_iterator& rhs) const noexcept {
				return this->_pointer == rhs._pointer;
			};

			[[nodiscard]] inline bool operator!=(const VectorTree_iterator& rhs) const noexcept {
				return !(*this == rhs);
			};

			[[nodiscard]] inline bool operator<(const VectorTree_iterator& rhs) const noexcept {
				return this->_index < rhs._index;
			};

			[[nodiscard]] inline bool operator<=(const VectorTree_iterator& rhs) const noexcept {
				return this->_index <= rhs._index;
			};

			[[nodiscard]] inline bool operator>(const VectorTree_iterator& rhs) const noexcept {
				return this->_index > rhs._index;
			};

			[[nodiscard]] inline bool operator>=(const VectorTree_iterator& rhs) const noexcept {
				return this->_index >= rhs._index;
			};
		};

		// local aliases
		using value_type = T;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using iterator = VectorTree_iterator<VectorTree>;
		using reference = T&;
		using _u_composite = VectorTree_node<VectorTree_node_type_composite>;
		using _u_leaf = VectorTree_node<VectorTree_node_type_leaf>;
		using _u_node_sh_var = std::variant<std::shared_ptr<_u_composite>, std::shared_ptr<_u_leaf>>;
		using _u_root = std::shared_ptr<_u_composite>;

		// members
		_u_root _root{ std::make_shared<_u_composite>(1) };
		unsigned char _height{ 1 };
		std::size_t _size{};

		[[nodiscard]] inline iterator begin() noexcept {
			if (empty()) { return end(); }
			auto leaf_node{ get_leaf_node(get_path_to_leaf_node(0)) };
			return iterator(this, leaf_node, &this->operator[](0));
		};
		[[nodiscard]] inline iterator end() noexcept {
			auto leaf_node{ get_leaf_node(get_path_to_leaf_node(_size)) };
			return iterator(this, leaf_node, one_past_the_last_element());
		};

		/*!
		 * @brief helper function to get the one past the last element iterator
		 */
		[[nodiscard]] inline auto one_past_the_last_element_const() const noexcept -> const_pointer {
			auto leaf_node{ get_leaf_node(get_path_to_leaf_node(_size)) };
			return leaf_node->_childs.end()._Ptr;
		};
		[[nodiscard]] inline auto one_past_the_last_element() noexcept -> pointer {
			auto leaf_node{ get_leaf_node(get_path_to_leaf_node(_size)) };
			return leaf_node->_childs.end()._Ptr;
		};

		/*!
		 * @brief helper function for random access
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto get_path_to_leaf_node(std::size_t index) const
			-> std::vector<std::size_t>
		{
			auto index_{ index };
			std::vector<std::size_t> path_to_leaf_node;
			for (auto i = 0; i < _height; ++i) {
				std::size_t level_count{ static_cast<std::size_t>(std::pow(BufferSize, _height - i)) };
				std::size_t level_index{ index_ / level_count };
				path_to_leaf_node.push_back(level_index);
				index_ -= level_index * level_count;
			}
			return path_to_leaf_node;
		};

		/*!
		 * @brief helper function for random access
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto get_path_to_element(std::size_t index) const
			-> std::vector<std::size_t>
		{
			auto index_{ index };
			std::vector<std::size_t> path_to_element;
			for (auto i = 0; i < _height; ++i) {
				std::size_t level_count{ static_cast<std::size_t>(std::pow(BufferSize, _height - i)) };
				std::size_t level_index{ index_ / level_count };
				path_to_element.push_back(level_index);
				index_ -= level_index * level_count;
			}
			path_to_element.push_back(index_);
			return path_to_element;
		};

		/*!
		 * @brief helper function in order to locate the next leaf node
		 * when the current leaf node has reached its capacity (BufferSize)
		 *
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto get_path_to_next_leaf_node(
			const std::vector<std::size_t>& path_to_current_leaf_node) const
			-> std::vector<std::size_t>
		{
			auto path_to_next_leaf_node = path_to_current_leaf_node;
			bool check{};
			for (auto i = _height - 1; i >= 0; --i) {
				if (path_to_next_leaf_node[i] < BufferSize - 1) {
					check = true;
					path_to_next_leaf_node[i]++;
					for (auto j = i + 1; j < _height; ++j) {
						path_to_next_leaf_node[j] = 0;
					}
				}
			}
			if (!check) { path_to_next_leaf_node = std::vector<std::size_t>(); }
			return path_to_next_leaf_node;
		};

		/*!
		 * @brief gets the pointer to the leaf node with the given path to the node
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto get_leaf_node(
			const std::vector<std::size_t>& path_to_leaf_node) const
			-> _u_leaf*
		{
			_u_composite* composite_raw = _root.get();
			for (auto i = 0; i < _height - 1; ++i) {
				auto& composite_sh_var{ composite_raw->_childs[path_to_leaf_node[i]] };
				auto& composite_sh{ std::get<std::shared_ptr<_u_composite>>(composite_sh_var) };
				composite_raw = composite_sh.get();
			}
			auto& leaf_sh_var{ composite_raw->_childs[path_to_leaf_node[_height - 1]] };
			auto& leaf_sh{ std::get<std::shared_ptr<_u_leaf>>(leaf_sh_var) };
			return leaf_sh.get();
		};

		/*!
		 * @brief Gets the pointer to the element with the given path to element
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] inline auto get_element(
			const std::vector<std::size_t>& path_to_element) const -> reference
		{
			auto leaf_node{ get_leaf_node(path_to_element) };
			return leaf_node->_childs[path_to_element[_height]];
		};

		/*!
		 * @brief copies all the nodes involved in the path to the given leaf node
		 * this is the basic approach of a persistent VectorTree.
		 * copy and update only the nodes which affected from the operation (push, emplace or pop)
		 * and increment the shared reference count to the remaining nodes.
		 * this approach reduces the amount of the data copied by logN, N being BufferSize.
		 *
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto copy_nodes_in_the_path_to_leaf_node_1(
			const std::vector<std::size_t>& path_to_leaf_node) const
			-> VectorTree<T, BufferSize>
		{
			auto new_root_sh{ std::make_shared<_u_composite>(*this->_root.get()) };
			auto new_composite_raw{ new_root_sh.get() };
			for (auto i = 0; i < _height - 1; ++i) {
				auto child_index{ path_to_leaf_node[i] };
				auto& child_composite_sh_var{ new_composite_raw->_childs[child_index] };
				auto& child_composite_sh{ std::get<std::shared_ptr<_u_composite>>(child_composite_sh_var) };
				auto new_child_composite_sh = std::make_shared<_u_composite>(*child_composite_sh);
				new_composite_raw->_childs[child_index] = _u_node_sh_var(new_child_composite_sh);
				new_composite_raw = new_child_composite_sh.get();
			}
			auto& child_leaf_sh_var{ new_composite_raw->_childs[path_to_leaf_node[_height - 1]] };
			auto& child_leaf_sh{ std::get<std::shared_ptr<_u_leaf>>(child_leaf_sh_var) };
			auto new_child_leaf_sh = std::make_shared<_u_leaf>(*child_leaf_sh);
			new_composite_raw->_childs[path_to_leaf_node[_height - 1]] = _u_node_sh_var(new_child_leaf_sh);

			return VectorTree<T, BufferSize>(new_root_sh, _height, _size);
		};

		/*!
		 * @brief copy_nodes_in_the_path_to_leaf_node_1 for two nodes
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto copy_nodes_in_the_path_to_leaf_node_2(
			const std::vector<std::size_t>& path_to_leaf_node_1,
			const std::vector<std::size_t>& path_to_leaf_node_2) const
			-> VectorTree<T, BufferSize>
		{
			auto new_root_sh{ std::make_shared<_u_composite>(*this->_root.get()) };
			auto new_composite_raw{ new_root_sh.get() };
			for (auto i = 0; i < _height - 1; ++i) {
				auto child_index{ path_to_leaf_node_1[i] };
				auto& child_composite_sh_var{ new_composite_raw->_childs[child_index] };
				auto& child_composite_sh{ std::get<std::shared_ptr<_u_composite>>(child_composite_sh_var) };
				auto new_child_composite_sh = std::make_shared<_u_composite>(*child_composite_sh);
				new_composite_raw->_childs[child_index] = _u_node_sh_var(new_child_composite_sh);
				new_composite_raw = new_child_composite_sh.get();
			}
			auto& child_leaf_sh_var{ new_composite_raw->_childs[path_to_leaf_node_1[_height - 1]] };
			auto& child_leaf_sh{ std::get<std::shared_ptr<_u_leaf>>(child_leaf_sh_var) };
			auto new_child_leaf_sh{ std::make_shared<_u_leaf>(*child_leaf_sh) };
			new_composite_raw->_childs[path_to_leaf_node_1[_height - 1]] = _u_node_sh_var(new_child_leaf_sh);

			new_composite_raw = new_root_sh.get();
			for (auto i = 0; i < _height - 1; ++i) {
				auto child_index{ path_to_leaf_node_2[i] };
				auto& child_composite_sh_var{ new_composite_raw->_childs[child_index] };
				auto& child_composite_sh{ std::get<std::shared_ptr<_u_composite>>(child_composite_sh_var) };
				auto new_child_composite_sh = std::make_shared<_u_composite>(*child_composite_sh);
				new_composite_raw->_childs[child_index] = _u_node_sh_var(new_child_composite_sh);
				new_composite_raw = new_child_composite_sh.get();
			}
			child_leaf_sh_var = new_composite_raw->_childs[path_to_leaf_node_2[_height - 1]];
			child_leaf_sh = std::get<std::shared_ptr<_u_leaf>>(child_leaf_sh_var);
			new_child_leaf_sh = std::make_shared<_u_leaf>(*child_leaf_sh);
			new_composite_raw->_childs[path_to_leaf_node_2[_height - 1]] = _u_node_sh_var(new_child_leaf_sh);

			return VectorTree<T, BufferSize>(new_root_sh, _height, _size);
		};

		/*!
		 * @brief helper function to append (push or emplace) a new element
		 * possible cases:
		 *     1. the VectorTree is empty
		 *     2. Otherwise: the current leaf node has room for a new entity
		 *     3. Otherwise: the root node has a room for a new entity
		 *     4. Otherwise: VectorTree is full, the height of the VectorTree is increased by a new root
		 *
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto append_helper_main() const -> VectorTree<T, BufferSize>
		{
			// case 1
			if (empty()) {
				return append_helper_case_1();
			}

			// case 2
			auto path_to_current_leaf_node{ get_path_to_leaf_node(_size - 1) };
			if (auto leaf_node{ get_leaf_node(path_to_current_leaf_node) }; 
				leaf_node->_childs.size() < BufferSize)
			{
				return append_helper_case_2(path_to_current_leaf_node);
			}

			// case 3
			if (auto path_to_next_leaf_node{ get_path_to_next_leaf_node(path_to_current_leaf_node) };
				!path_to_next_leaf_node.empty())
			{
				return append_helper_case_3();
			}

			// case 4
			return append_helper_case_4();
		};

		/*!
		 * @see the documentation of append_helper_main
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] inline auto append_helper_case_1() const
			-> VectorTree<T, BufferSize>
		{
			auto new_root_sh = std::make_shared<_u_composite>(1);
			return VectorTree<T, BufferSize>(new_root_sh, _height, _size);
		};

		/*!
		 * @see the documentation of append_helper_main
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] inline auto append_helper_case_2(
			const std::vector<std::size_t>& path_to_leaf_node) const
			-> VectorTree<T, BufferSize>
		{
			return copy_nodes_in_the_path_to_leaf_node_1(path_to_leaf_node);
		};

		/*!
		 * @see the documentation of append_helper_main
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] inline auto append_helper_case_3() const
			-> VectorTree<T, BufferSize>
		{
			auto new_root_sh{ std::make_shared<_u_composite>(*this->_root.get()) };
			new_root_sh->_childs.emplace_back(std::make_shared<_u_composite>(_height - 1));
			return VectorTree<T, BufferSize>(new_root_sh, _height, _size);
		};

		/*!
		 * @see the documentation of append_helper_main
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] inline auto append_helper_case_4() const -> VectorTree<T, BufferSize>
		{
			auto new_root_sh{ std::make_shared<_u_composite>() };
			new_root_sh->_childs.emplace_back(_root);
			new_root_sh->_childs.emplace_back(std::make_shared<_u_composite>(_height));
			return VectorTree<T, BufferSize>(new_root_sh, _height + 1, _size);
		};

	public:

		// STL aliases
		using value_type = T;
		using allocator_type = Allocator;
		using pointer = value_type*;
		using const_pointer = const value_type*;
		using iterator = VectorTree_iterator<VectorTree>;
		using const_iterator = VectorTree_const_iterator<VectorTree>;
		using reference = T&;
		using const_reference = const T&;
		using size_type = std::size_t;
		using difference_type = std::ptrdiff_t;

		VectorTree() = default;
		explicit VectorTree(std::size_t s) : _size{ s } {
			for (auto i = MAX_VectorTree_HEIGHT; i > 0; i--) {
				std::size_t level_count{ static_cast<std::size_t>(std::pow(BufferSize, i)) };
				if (level_count < s) {
					_height = i + 1;
					break;
				}
			}
			_root = std::make_shared<_u_composite>(_height);
		};
		VectorTree(_u_root root, unsigned char height, std::size_t s) :
			_root{ root }, _height{ height }, _size{ s } {};

		/*!
		 * @brief the big 5
		 * apply copy-and-swap idiom for the assignment operator
		 * notice that the root node is cloned (i.e. deep copy)
		 * instead of incrementing the shared reference counts
		 */
		VectorTree(const VectorTree& rhs)
			:
			_root{ rhs._root->clone() },
			_height{ rhs._height },
			_size{ rhs._size } {};
		VectorTree& operator=(VectorTree rhs) {
			swap(*this, rhs);
			return *this;
		};
		VectorTree(VectorTree&& rhs) noexcept = default;
		~VectorTree() = default;

		/*!
		 * @brief friend swap function
		 */
		friend inline void swap(VectorTree& lhs, VectorTree& rhs) noexcept {
			using std::swap;
			swap(lhs._root, rhs._root);
			swap(lhs._height, rhs._height);
			swap(lhs._size, rhs._size);
		};

		/*!
		 * @brief random access operator
		 * creates a vector to locate the path to the input index.
		 * hence, theoritically is not noexcept.
		 * but the size of the vector is very small
		 * (less than MAX_VectorTree_HEIGHT)
		 */
		[[nodiscard]] inline const_reference operator[](std::size_t index) const {
			return get_element(get_path_to_element(index));
		};

		[[nodiscard]] inline auto operator<=>(const VectorTree& rhs) const noexcept {
			return size() <=> rhs.size();
		};
		[[nodiscard]] inline bool operator==(const VectorTree& rhs) const noexcept = default;

		[[nodiscard]] inline const_iterator cbegin() const {
			const_iterator it;
			if (empty()) { it = cend(); }
			else {
				auto leaf_node{ get_leaf_node(get_path_to_leaf_node(0)) };
				it = const_iterator(this, leaf_node, operator[](0));
			}
			return it;
		};
		[[nodiscard]] inline const_iterator cend() const {
			auto leaf_node{ get_leaf_node(get_path_to_leaf_node(_size)) };
			return const_iterator(this, leaf_node, one_past_the_last_element_const());
		};

		[[nodiscard]] inline std::size_t size() const noexcept { return _size; };
		[[nodiscard]] inline bool empty() const noexcept {
			return _size == 0;
		};
		[[nodiscard]] inline reference back() const {
			return get_element(get_path_to_element(_size - 1));
		};

		/*!
		 * @see the documentation of append_helper_main for the details
		 * @exceptsafe strong exception safety as non-mutating
		 */
		template <typename U = T>
		[[nodiscard]] inline auto push_back(U&& t) const -> VectorTree<T, BufferSize>
		{
			auto new_vt{ append_helper_main() };

			std::size_t index{ 0 };
			if (_size > 0) { index = _size - 1; }
			auto leaf_node{ new_vt.get_leaf_node(new_vt.get_path_to_leaf_node(index)) };
			leaf_node->_childs.push_back(std::forward<U>(t));

			new_vt._size++;
			return new_vt;
		};

		/*!
		 * @see the documentation of append_helper_main for the details
		 * @exceptsafe strong exception safety as non-mutating
		 */
		template <typename... Ts>
		[[nodiscard]] inline auto emplace_back(Ts&& ... ts) const -> VectorTree<T, BufferSize>
		{
			auto new_vt{ append_helper_main() };

			std::size_t index{ 0 };
			if (_size > 0) { index = _size - 1;  }
			auto leaf_node{ new_vt.get_leaf_node(new_vt.get_path_to_leaf_node(index)) };
			leaf_node->_childs.emplace_back(std::forward<Ts>(ts)...);

			new_vt._size++;
			return new_vt;
		};

		[[nodiscard]] auto pop_back() const -> VectorTree<T, BufferSize> {
			if (empty()) {
				throw std::logic_error("Cannot pop an empty container.");
			}

			VectorTree<T, BufferSize> new_vt;
			if (_size > 1) {
				auto path_to_leaf_node{ get_path_to_leaf_node(_size - 1) };
				new_vt = copy_nodes_in_the_path_to_leaf_node_1(path_to_leaf_node);
				auto leaf_node{ new_vt.get_leaf_node(new_vt.get_path_to_leaf_node(_size - 1)) };
				leaf_node->_childs.pop_back();
				new_vt._size--;
			}
			return new_vt;
		};

		/*!
		 * @brief NOT EFFICIENT for VectorTree
		 */
		[[deprecated("Inserting at the middle is not effective for VectorTree")]]
		auto insert(std::size_t, T) const -> VectorTree<T, BufferSize> {
			/// TODO
		};

		/*!
		 * @brief NOT EFFICIENT for VectorTree
		 */
		[[deprecated("Erasing single element at the middle is not effective for VectorTree. Prefer erase__by_swap_and_pop")]]
		auto erase(const_iterator it) const -> VectorTree<T, BufferSize> {
			/// TODO
		};

		/*!
		 * @brief erase an element at the middle by:
		 *     1. copy the nodes from the root to the leaf node containing the input element
		 *     2. swap the last element with the requested element
		 *     3. pop the last element of the new VT
		 * the order of all elements accept for the last element is reserved.
		 * hence, only the index of the last element is invalidated.
		 *
		 * @exceptsafe strong exception safety as non-mutating
		 */
		[[nodiscard]] auto erase__by_swap_and_pop(std::size_t index) const -> VectorTree<T, BufferSize> {
			if (index >= _size) {
				throw std::logic_error("index out of bounds.");
			}

			// copy the nodes from the root to the leaf node containing the input element
			auto path_to_leaf_node__index{ get_path_to_leaf_node(index) };
			auto path_to_leaf_node__last{ get_path_to_leaf_node(_size - 1) };
			VectorTree<T, BufferSize> new_vt;
			if (path_to_leaf_node__index == path_to_leaf_node__last) {
				new_vt = copy_nodes_in_the_path_to_leaf_node_1(path_to_leaf_node__index);
			}
			else {
				new_vt = copy_nodes_in_the_path_to_leaf_node_2(
					path_to_leaf_node__index,
					path_to_leaf_node__last);
			}

			// swap
			auto& new_data__index{ new_vt.get_element(new_vt.get_path_to_element(index)) };
			auto& new_data__last{ new_vt.get_element(new_vt.get_path_to_element(_size - 1)) };
			std::swap(new_data__index, new_data__last);

			// pop
			auto leaf_node{ new_vt.get_leaf_node(new_vt.get_path_to_leaf_node(_size - 1)) };
			leaf_node->_childs.pop_back();
			--new_vt._size;

			return new_vt;
		};

		/*!
		 * @brief setter for the contained data
		 * @exceptsafe strong exception safety as non-mutating
		 */
		template <typename U = T>
		[[nodiscard]] auto set_data(std::size_t index, U&& data) const -> VectorTree<T, BufferSize> {
			if (index >= _size) {
				throw std::logic_error("index out of bounds.");
			}

			/// copy the nodes from the root to the leaf node containing the input element
			auto new_vt{ copy_nodes_in_the_path_to_leaf_node_1(get_path_to_leaf_node(index)) };
			auto& new_data{ new_vt.get_element(new_vt.get_path_to_element(index)) };
			new_data = std::forward<U>(data);

			return new_vt;
		};

		/*!
		 * @brief STL for_each algorithm
		 * clone this VectorTree and apply the input function on the clone
		 *
		 * @exceptsafe strong exception safety as non-mutating
		 */
		template<class UnaryFunc>
		[[nodiscard]] auto for_each(UnaryFunc f) const -> VectorTree<T, BufferSize>
		{
			VectorTree<T, BufferSize> new_vt{ *this };
			for (auto it = new_vt.begin(); it != new_vt.end(); ++it) {
				f(*it);
			}
			return new_vt;
		};
	};
}

#endif
