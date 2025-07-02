/*!
 * @brief a queue-like read-only wrapper interface
 * Provides a queue-like read-only access
 * to the containers having random access iterator
 * without allocating a new memory.
 * Therefore, has a very simple interface
 * with an explicit constructor and
 * the following two member functions: empty and front.
 * 8 specializations deals with the following cases:
 *     Container<T, N>                                => ex: std::array<T, N>
 *     Container<T*, N>                               => ex: std::array<T*, N>
 *     Container<UniquePtr<T, Deleter>, N>            => ex: std::array<std::unique_ptr<T, std::default_delete>, N>
 *     Container<SharedPtr<T>, N>                     => ex: std::array<std::shared_ptr<T>, N>
 *     Container<T, Allocator>                        => ex: std::vector<T, std::allocator<T>>
 *     Container<T*, Allocator>                       => ex: std::vector<T*, std::allocator<T*>>
 *     Container<UniquePtr<T, Deleter>, Allocator>    => ex: std::vector<std::unique_ptr<T, std::default_delete>, std::allocator<std::unique_ptr<T, std::default_delete>>>
 *     Container<SharedPtr<T>, Allocator>             => ex: std::vector<std::shared_ptr<T>, std::allocator<std::unique_ptr<T, std::default_delete>>>
 * empty andd front member functins must be coupled
 * as the index is not inspected internally.
 * @author    <baris.albayrak.ieee@gmail.com>
 * @version   0.0.a
 * @see       GeometryApplication.h FOR THE MAIN DOCUMENTATION OF THIS PROJECT
 * 
 * github: https://github.com/BarisAlbayrakIEEE/cpp.git
 */

#ifndef _read_only_queue_wrapper_HeaderFile
#define _read_only_queue_wrapper_HeaderFile

#include <cstddef>
#include <type_traits>
#include <iterator>
#include <concepts>

namespace ROQRNamespace {
	template <
		typename T,
		std::size_t N,
		template <typename, std::size_t> typename Container>
	concept CRandomAccess_1 = std::is_same_v<
		typename std::iterator_traits<typename Container<T, N>::iterator>::iterator_category,
		std::random_access_iterator_tag>;
	template <
		typename T,
		std::size_t N,
		template <typename> typename Deleter,
		template <typename, typename> typename UniquePtr,
		template <typename, std::size_t> typename Container>
	concept CRandomAccess_3 = std::is_same_v<
		typename std::iterator_traits<typename Container<UniquePtr<T, Deleter<T>>, N>::iterator>::iterator_category,
		std::random_access_iterator_tag>;
	template <
		typename T,
		std::size_t N,
		template <typename> typename SharedPtr,
		template <typename, std::size_t> typename Container>
	concept CRandomAccess_4 = std::is_same_v<
		typename std::iterator_traits<typename Container<SharedPtr<T>, N>::iterator>::iterator_category,
		std::random_access_iterator_tag>;
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename, typename> typename Container>
	concept CRandomAccess_5 = std::is_same_v<
		typename std::iterator_traits<typename Container<T, Allocator<T>>::iterator>::iterator_category,
		std::random_access_iterator_tag>;
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename> typename Deleter,
		template <typename, typename> typename UniquePtr,
		template <typename, typename> typename Container>
	concept CRandomAccess_7 = std::is_same_v<
		typename std::iterator_traits<typename Container<UniquePtr<T, Deleter<T>>, Allocator<UniquePtr<T, Deleter<T>>>>::iterator>::iterator_category,
		std::random_access_iterator_tag>;
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename> typename SharedPtr,
		template <typename, typename> typename Container>
	concept CRandomAccess_8 = std::is_same_v<
		typename std::iterator_traits<typename Container<SharedPtr<T>, Allocator<SharedPtr<T>>>::iterator>::iterator_category,
		std::random_access_iterator_tag>;

	/*!
	 * @brief a queue-like read-only wrapper class 
	 * base template
	 * @see the main documentation of this header for the details
	 */
	template <typename Container>
	struct read_only_queue_wrapper {};
	
	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<T, N>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		std::size_t N,
		template <typename, std::size_t> typename Container>
		requires (CRandomAccess_1<T, N, Container>)
	struct read_only_queue_wrapper<Container<T, N>> {
		using pointer = T const*;
		using _u_container_t = Container<T, N> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == N;
		};
		[[nodiscard]] inline auto front() -> pointer {
			return &_container->operator[](_index++);
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<T*, N>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		std::size_t N,
		template <typename, std::size_t> typename Container>
		requires (CRandomAccess_1<T, N, Container>)
	struct read_only_queue_wrapper<Container<T*, N>> {
		using pointer = T const*;
		using _u_container_t = Container<T*, N> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == N;
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++);
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<UniquePtr<T, Deleter>, N>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		std::size_t N,
		template <typename> typename Deleter,
		template <typename, typename> typename UniquePtr,
		template <typename, std::size_t> typename Container>
		requires (CRandomAccess_3<T, N, Deleter, UniquePtr, Container>)
	struct read_only_queue_wrapper<Container<UniquePtr<T, Deleter<T>>, N>> {
		using pointer = T const*;
		using _u_container_t = Container<UniquePtr<T, Deleter<T>>, N> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == N;
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++).get();
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<SharedPtr<T>, N>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		std::size_t N,
		template <typename> typename SharedPtr,
		template <typename, std::size_t> typename Container>
		requires (CRandomAccess_4<T, N, SharedPtr, Container>)
	struct read_only_queue_wrapper<Container<SharedPtr<T>, N>> {
		using pointer = T const*;
		using _u_container_t = Container<SharedPtr<T>, N> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == N;
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++).get();
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<T, Allocator>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename, typename> typename Container>
		requires (CRandomAccess_5<T, Allocator, Container>)
	struct read_only_queue_wrapper<Container<T, Allocator<T>>> {
		using pointer = T const*;
		using _u_container_t = Container<T, Allocator<T>> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == _container->size();
		};
		[[nodiscard]] inline auto front() -> pointer {
			return &_container->operator[](_index++);
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<T*, Allocator>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename, typename> typename Container>
		requires (CRandomAccess_5<T, Allocator, Container>)
	struct read_only_queue_wrapper<Container<T*, Allocator<T*>>> {
		using pointer = T const*;
		using _u_container_t = Container<T*, Allocator<T*>> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == _container->size();
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++);
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<UniquePtr<T, Deleter>, Allocator>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename> typename Deleter,
		template <typename, typename> typename UniquePtr,
		template <typename, typename> typename Container>
		requires (CRandomAccess_7<T, Allocator, Deleter, UniquePtr, Container>)
	struct read_only_queue_wrapper<Container<UniquePtr<T, Deleter<T>>, Allocator<UniquePtr<T, Deleter<T>>>>> {
		using pointer = T const*;
		using _u_container_t = Container<UniquePtr<T, Deleter<T>>, Allocator<UniquePtr<T, Deleter<T>>>> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == _container->size();
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++).get();
		};
	};

	/*!
	 * @brief a queue-like read-only wrapper class
	 * specialization:
	 *     container: Container<SharedPtr<T>, Allocator>
	 * @see the main documentation of this header for the details
	 */
	template <
		typename T,
		template <typename> typename Allocator,
		template <typename> typename SharedPtr,
		template <typename, typename> typename Container>
		requires (CRandomAccess_8<T, Allocator, SharedPtr, Container>)
	struct read_only_queue_wrapper<Container<SharedPtr<T>, Allocator<SharedPtr<T>>>> {
		using pointer = T const*;
		using _u_container_t = Container<SharedPtr<T>, Allocator<SharedPtr<T>>> const*;

		_u_container_t _container;
		std::size_t _index{};

		explicit read_only_queue_wrapper(_u_container_t container)
			: _container(container) {};

		[[nodiscard]] inline auto empty() const -> bool {
			return _index == _container->size();
		};
		[[nodiscard]] inline auto front() -> pointer {
			return _container->operator[](_index++).get();
		};
	};
};

#endif
