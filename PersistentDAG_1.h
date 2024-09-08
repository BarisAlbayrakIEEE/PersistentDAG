/*!
 * A persistent DAG interface
 *
 * REQUIREMENTS:
 * 1. The contained type T must satisfy the following interface:
 *    std::copy_constructible<T>
 *    std::is_move_assignable<T>
 *    void set_DAG_node(_a_node_var_raw)
 *    bool inspect_invariant() (case dependent: invariant_updatable)
 * 2. T objects should not have pointers to each other.
 *    As a persistent data structure,
 *    the DAG relies on the structural sharing for the contained type T
 *    which performs copy construction on the objects of T.
 *    Hence, if T objects have pointers to other T objects,
 *    the structural sharing will just copy the pointers.
 *    Therefore, reconsider using this interface in such a case.
 *    The issue is explained in DATA STRUCTURES AND MEMORY MANAGEMENT section.
 *
 *
 *
 * ASSUMPTIONS AND LIMITATIONS:
 * The only aim of the DAG interface in this header file
 * is to store the data (T) in a directed graph data structure
 * while keeping a state of the data uptodate.
 * hence, many fundamental operations of a DAG are excluded.
 * Two important parameters are missing in this DAG interface:
 *     1. Edges
 *     2. Node weight factors
 * Edges are simulated in node relation data (i.e. ancestor nodes and descendant nodes).
 * Node weight factors are not defined
 * which means that all nodes are treated equally.
 * Therefore, this is not a generalized DAG data structure
 * and many graph algorithms (e.g. shortest path) are missing.
 * 
 * 
 * 
 * CAUTION:
 * the next version of this DAG interface is implemented in PersistentDAG_2.h.
 * this version does not define a node class.
 * however, the next version defines an inner node class
 * templated by two parameters:
 *     updatability type and ancestor node count type
 * this templated inner node class provides mainly two enhancements:
 *     1. updatibility type is a part of node definition (i.e. static type definition)
 *        which replaces the runtime updatability definition in this version
 *     2. ancestor node count type enhances the memory allocation
 *        and the efficiency of the copy constructor of the DAG
 *
 *
 *
 * INTRODUCTION:
 * Graph is a kind of dynamic linked data structure.
 * It allows both many-to-one and one-to-many relationships
 * unlike a tree which only allows one-to-many.
 * As a result of the many-to-one relation,
 * a graph may have more than one paths between two nodes.
 * A graph data structure may be directed or not.
 * The DAG here implements the ancestor and descendant directions topologically
 * which yields a directed graph data structure.
 * The topological difference with the ancestor and descendant relations
 * is not realized by definition (both use std::vector)
 * but hidden in the algorithms conceptually.
 * this is a result of the procedural approach
 * followed in the definition of the DAG.
 * i will explain the issue in detail in the following paragraphs.
 *
 * the invariant of the DAG requires the absence of directed cycles.
 * a cycle is a path with more than one node
 * for which the beginning and the end nodes are the same.
 * the DAG implementation in this header file allows directed cycles
 * in an invalid state.
 * in other words, when a node is involved in a directed cycle,
 * the state of the DAG becomes invalid until the cycle is defeated.
 * the algorithms are immune to the cycles and
 * the iterations skip the cycled nodes.
 * in summary, as stated above, the DAG allows directed cycles
 * in an invalid state.
 * so, why not calling Directed Graph (DG) if cycles are allowed.
 * the reason is to emphasize that the cycles are not wanted.
 * the algorithms are designed to keep the state of the DAG against the
 * directed cycles.
 * when a user action yields a directed cycle,
 * the DAG terminates the action and inform the user about the cycle
 * formation.
 * the user intentionally can insist on the action causing the cycle.
 * then, the action is executed and the DAG will take care of the cycle
 * waiting for the user to terminate the cycle by modifying the node
 * relations.
 *
 * as stated for the directed cycles,
 * this DAG implementation defines a state
 * for each node.
 * the power of this DAG implementation is that
 * it covers all the possible states,
 * couples the state data with the node relations and
 * adjusts the algorithms according to the state data.
 * followings are the possible states for a node:
 *     1. uptodate:
 *        the only valid state for a DAG node.
 *        a DAG is in a valid state if all the nodes are uptodate.
 *     2. invalid:
 *        THE INVALID STATE IS A RESULT OF AN EXTERNAL DEFINITION
 *        WHICH IS ONE OF THE TWO INTERFACES OF THE DAG WITH THE CONTAINED
 *        TYPE T.
 *        the conditions causing the invalid state must be defined by T.
 *        in other words, invalid state simulates the invariant of T.
 *        for example, consider the DAG is used by a geometry application
 *        which defines Vector type.
 *        the Vector object is defined by 3 components in the space
 *        and at least one of them must be non-zero by the Vector invariant.
 *        a user action which makes all the 3 components zero
 *        causes the node of the Vector to have invalid state.
 *     3. ancestor_fail:
 *        means that one or more of the ancestor nodes of a node is not uptodate.
 *        the node causing the ancestor_fail state may not be a direct
 *        ancestor of the node
 *        as this state is propagated through the whole DAG along the
 *        descendant paths.
 *        this is one of the hidden topological differences
 *        between the ancestor and descendant definitions mentioned in the
 *        2nd paragraph.
 *     4. cycled:
 *        this is the state of a node involved in a directed cycle
 *        and happens if the user insists on the action.
 *        the algorithms are immune to the cycles and the iterations skip
 *        the cycled nodes.
 *        the user can terminate a directed cycle by modifying the relations
 *        of the nodes.
 *     5. deleted:
 *        this is another exceptional case like cycled state.
 *        descendant relations is the source of the memory management
 *        where the usual implementation of a pointer based DAG defines the
 *        ownership.
 *        however, this DAG implementation
 *        is based on the indices instead of pointers
 *        which prevents defining a node
 *        with the ownership of the descendant nodes.
 *        hence, the ownership relation in the descendant direction
 *        is not by definition
 *        but injected into the algorithms conceptually.
 *        this is the 2nd hidden topological difference
 *        between the ancestor and descendant definitions mentioned in the
 *        2nd paragraph.
 *        by definition, the nodes without descendant nodes can be deleted.
 *        by definition, the nodes with descendant nodes can be deleted
 *        only together with the descendant nodes.
 *        however, deleting a node without deleting the descendant nodes is
 *        an invalid operation.
 *        but, the DAG allows this invalid operation
 *        by setting the state of the node as deleted.
 *        the algorithms are immune to the deleted nodes and the iterations
 *        skip them.
 *        after the user clears the descendant nodes of the deleted node
 *        (by deleting the descendant nodes or by replacing the ancestor nodes of them)
 *        the DAG removes the node safely.
 *
 * the manipulation of the state of a DAG node
 * requires another property that a node needs to have: updatability
 * followings are the possible types for the updatability of a node:
 *     1. non_updatable
 *        the node has no updatability issue and always has uptodate state.
 *        these nodes, by definition, cannot have ancestor nodes.
 *     2. self_updatable
 *        similar to non_updatable type, the node has no updatability issue
 *        and always has uptodate state.
 *        these nodes, by definition, does not have ancestor nodes.
 *        for example, a Point in a geometry application is self-updatable
 *        as it does not have ancestor nodes and an invariant.
 *     3. ancestor_updatable
 *        the state of the node depends on the state of the ancestor nodes.
 *        the state is uptodate if all the ancestor nodes are uptodate,
 *        otherwise, ancestor_fail.
 *        the node itself does not have any invariant
 *     4. invariant_updatable
 *        the state of the node depends on
 *        the state of the ancestor nodes (if exists)
 *        and the invariant of the contained type T.
 * 
 * this version defines the updatability as an enumeration
 * which is assigned to each node individually.
 * However, this definition makes the updatability a runtime issue
 * although in reality its usually a part of the definition.
 * the next version of this interface (PersistentDAG_2.h) solves this problem
 * by defining an inner node class templated by the updatability type.
 * see PersistentDAG_2.h for the details.
 * 
 * As stated above, the most important feature of this DAG
 * is the state data managed for all nodes
 * together with the node relations.
 * lets inspect two of the basic algorithms: node creation and modification.
 * when a new entity is added,
 * the DAG creates a node for the entity.
 * the current paths are just extended by this action
 * as a new node cannot have descendant nodes.
 * the DAG inspects the ancestors of the new node
 * if the node is required to have ancestor nodes.
 * if the ancestors are all uptodate
 * the state of the new node is also uptodate.
 * otherwise ancestor_fail.
 * if the states of the ancestors are all uptodate,
 * the invariant of the contained type T is performed
 * if T is invariant_updatable.
 * this is the whole operation for node creation.
 * on the other hand,
 * in case of an update action (e.g. modifying the ancestors of a node),
 * the paths in the current DAG are being modified
 * which requires an inspection following the modified descendant paths.
 * in the worst case,
 * the length of the inspection approaches to the whole DAG
 * when the modified node approaches to the head node.
 * a BFS traversal starting from the ancestors of the modified node is the best choice
 * as the BFS is a level based algorithm
 * which ensures a healthy ancestor state inspection for each node.
 * BFS solves the problem in O(N)
 * while for DFS its O(N2)
 * as an additional traversal in ancestor direction is required
 * for each node traversed during the iteration in the descendant direction.
 * hence, a modification in the paths of the DAG
 * is the most complicated algorithm which has O(N) time complexity.
 * in most of the DAG implementations,
 * this algorithm (i.e. inspecting the nodes by a traversal in the DAG)
 * is executed by a background thread.
 * this DAG implementation relies on this approach
 * where a main thread performs the operations on the DAG
 * while a background thread keeps inspecting and updating the states of the nodes.
 * see the CONCURRENCY section for the details.
 * 
 *
 *
 * DATA STRUCTURES AND MEMORY MANAGEMENT:
 * DAG stores the contained data in VectorTree
 * which is a fully persistent data structure that relies on the data sharing.
 * see VectorTree.h for the details.
 * 
 * the DAG requires internal data structures
 * in order to define the node relations and the states.
 * additionally, some auxilary data is needed
 * such as the cycled nodes.
 * all the internal data is stored in std::vector
 * accept for the cycled nodes which is stored in std::unordered_map.
 * 
 * std::vector is the main container in STL and used widely.
 * hence, the properties of std::vector is well known.
 * however, a discussion about the contiguous containers in STL
 * would provide a better understanding of the DAG.
 * the DAG in this version stores the data in std::vector
 * and does not use std::array.
 * especially the members defined by std::vector<std::vector<std::size_t>>
 * would use the cache more efficiently
 * if the inner vector could be replaced by a std::array
 * (e.g. std::vector<std::array<std::size_t, 2>>)
 * as a vector of arrays would allocate a single contiguous memory.
 * the next version (PersistentDAG_2.h) replaces the use of a single
 *     std::vector<std::vector<std::size_t>>
 * with a number of
 *     std::vector<std::array<std::size_t, i>>
 *     where i = 1,...k.
 * the maximum value of k is defined statically.
 *
 * 
 * the indexing of the VectorTree and the vectors are all dependent/parallel.
 * the index dependency is secured by the persistency itself (i.e. public const functions).
 * however, the DAG contains some private mutating functions (e.g. update_state).
 * all private mutating functions satisfy the strong exception safety.
 * 
 * std::vector allocates a contiguous memory
 * which is highly efficient due to the efficient cache usage
 * for the two most critical procedures of a persistent DAG structure:
 *     1. copy construction
 *     2. traversal
 * 
 * in order to define the node relations,
 * the DAG uses indices instead of the pointers
 * so that the efficiency of the copy constructor of the DAG is improved.
 * because, copying a DAG with pointers
 * would require an algorithm which resets
 * the pointers to point to the new nodes.
 * such an algorithm would have a time complexity of O(kN)
 * additional to the copy operation
 * where N is the number of nodes.
 * Hence, the choice of using indices instead of the pointers
 * provides an enhancement for the copy construction.
 * the performance of the copy constructor increases more
 * with the trivially copyable objects
 * as std::vector guarantees the bitwise copy in such a case.
 * hence, std::vector<std::size_t> where std::size_t is for the node index
 * is the optimum data structure to define the node relations in a DAG.
 * on the other hand, index based approach
 * creates an additional load on the iteration
 * because the random access comes with an additional pointer indirection.
 * see ITERATORS andd CONCURRENCY sections for the details.
 * 
 * the relations within the DAG structure does not imply that
 * the objects of the contained type T are also related to each other
 * (e.g. via pointers).
 * actually, it implies that T should not define such relations as the DAG
 * already does.
 * as a result, T is thought to be independent of the relations defined in
 * the DAG.
 * hence, the structural sharing can be utilized for the contained type T
 * which minimizes the amount of copied data.
 * the structural sharing is crucial when T stores large data.
 *
 *
 *
 * ITERATORS:
 * STL style bidirectional iterator is defined as an inner class.
 * const iterator is public and
 * non-cocst iterator is private for internal usage.
 *
 * bidirectionality is provided by
 * the direction type template parameter (ancestor or descendant)
 *
 * performs looping instead of recursion
 * as the DAG is a large structure
 * which may result with stack overflow.
 *
 * defines all STL aliases (e.g. iterator_category, value_type, etc.)
 * and the required interface (e.g. increment operator, etc.).
 * thus, the iterator class satisfies the STL rules
 * and can be used safely with the STL algorithms.
 * 
 * the iterator stores the visited nodes (i.e. indices)
 * due to the many-to-one relations.
 * the use of indices instead of the pointers creates
 * an additional indirection during the iteration
 * which increases the runtime of the iteration.
 *     1. locate the beginning of the node vector's data
 *     2. locate the relative position of the node using random access operator.
 * both constant time but now we have two pointer indirections.
 * 
 * however, the index usage allows storing the visited nodes
 * into a std::vector instead of a std::unordered_map.
 * std::vector guarantees the constant time access
 * while unordered_map may suffer from hash collisions.
 * hence, the index usage provides more efficient iteration
 * for the large DAGs.
 * 
 * the iterators implements both BFS and DFS.
 * i will not go into details with the details of BFS and DFS
 * and the use cases for each.
 * however, the internal functions (e.g. state propogation, state update)
 * mainly uses BFS as it provides a level traversal.
 * 
 * as specified above,
 * the DAG iterator must store auxilary data (i.e. the visited nodes)
 * due to the many-to-one relationship.
 * hence, the iterator may throw bad_alloc exception.
 * the functions using the DAG iterator shall consider the exception safety.
 * the persistency provides strong exception safety for the public functions.
 * however, the DAG has two private mutating functions
 * to be executed by the background thread.
 *     1. update_DAG_state
 *     2. erase_deleted_nodes
 * the strong exception safety is achieved by storing a backup data
 * that would be modified during the function execution.
 * the backup data refers to
 *     the states of the nodes (copy is cheap) for update_DAG_state and,
 *     the node relation data (copy is not cheap) for erase_deleted_nodes.
 * hence, backup solution is not reasonable for erase_deleted_nodes.
 * the 2nd version (PersistentDAG_2.h)
 * embeds erase_deleted_nodes into erase function
 * without increasing the runtime complexity significantly.
 * 
 * 
 * 
 * 
 * CONCURRENCY:
 * as stated above in the INTRODUCTION section,
 * multithreading is required to maintain the state of the nodes
 * by a background thread.
 * as specified earlier,
 * if the relations of a node is modified by a user action
 * the node state data must be inspected and modified (if needed)
 * by a BFS traversal in the descendant direction
 * starting from the updated node.
 * the traversal starts from the head node
 * if the DAG contains a directed cycle.
 * this process is executed by the background thread.
 * 
 * for a concurrent implementation of a data structure
 * we have three approaches:
 *     1. lock-based mutable
 *     2. lock-free mutable (atomics)
 *     3. persistent immutable
 *
 * 
 * 1st (lock-based) approach:
 * the 1st (lock-based) approach requires
 * a fine-grained locking scheme
 * based on the nodes.
 * however, maintaining the fine granularity
 * for a DAG structure is not possible.
 * this is quite well-known but let me describe the details
 * as this application is a part of my resume.
 *
 * lets inspect lock-based doubly linked list data structure
 * before discussing the DAG.
 * when a node in the list is to be modified,
 * lock-based approach would lock the node together with the nodes on either
 * sides as all the three nodes are related to each other via pointers
 * and the thread safety requires
 * the safety of the pointers during the mutation.
 * however, the forward and backward traversals
 * through the doubly linked list
 * would end up with a dead lock.
 * consider a linked list with nodes A, B, C and D.
 * consider the forward traversal inspects node B and
 * the backward inspects node C.
 * a dead lock arises if the forward iteration acquires the lock on node B first
 * and the backward acquires the lock on node C first.
 * this can be solved by assigning a lock order in each direction.
 *
 * the DAG is also a linked data structure.
 * thus, the same discussion applies to the DAG also.
 * however, we have three problems for the above locking scheme:
 *     1. the ancestor/descendant node definitions are dynamic unlike a linked list:
 *        STL does not have a tool to lock mutexes in a dynamic container.
 *        on the other hand, boost::lock has an overload which supports
 *        std::vector<std::mutex>.
 *        however, STL has a reason while excluding the dynamic containers:
 *            * compile time locking ensures the RAII, exception safety and
 *              the intent of the lock while runtime does not
 *            * the locking process may include too many mutexes
 *              which is basically not reasonable and open to deadlocks.
 *              the STL's choice actually is a significant sign to give up the
 *              lock-based approach.
 *     2. dead locks would arise even in a traversal in the same direction:
 *        consider we have node N1 with descendant nodes N11, N12, N13, N100 and N200.
 *        consider we have node N2 with descendant nodes N21, N22, N23, N100 and N200.
 *        lets ignore the ancestor nodes for simplicity.
 *        consider two threads work on N1 and N2 respectively.
 *        both threads need to acquire the locks as specified in case of the
 *        doubly linked list.
 *        assume thread1 acquired the locks on N1, N11, N12, N13 and N100.
 *        assume thread2 acquired the locks on N2, N21, N22, N23 and N200.
 *        so thread1 waits for N200 holding N100 while
 *        thread2 waits for N100 holding N200.
 *        this is a deadlock arised in a traversal in the same direction
 *        even we did not consider the ancestor nodes.
 *        even worst, its not possible to define a locking order.
 *        the problem can be solved by switching to coarser locking (lock
 *        the whole DAG)
 *        or by algorithmically creating a pool of nodes
 *        from which the threads request the common node.
 *        the 1st one is not a choice and the 2nd one adds too much overhead
 *        especially when the number of nodes gets larger.
 *     3. the number of the nodes to be locked gets very large in a few cycle.
 *        locking too many mutexes (e.g. tens or hundreds of them) is not reasonable due
 *        to many reasons.
 *        in case of a DAG the number of descendant nodes can be any number even
 *        thousands.
 *        however, even the number of descendant nodes is not large for
 *        individual nodes,
 *        the locking scheme ends up locking too many nodes.
 *        consider again the case in the 2nd item.
 *        when a thread is working on N11, it needs to hold
 *        the locks on N1, N11 and the descendant nodes of N11.
 *        when a thread is working on N200, it needs to hold
 *        the locks on N1, N11, N12, N13, N100, N200 and the descendant nodes of all.
 *        the lock on N1 can be released only when all of its descendant nodes are examined.
 *        hence, as the iteration goes on,
 *        the number of new locks acquired is far more larger than the
 *        number of the released locks.
 *
 * another weakness of the lock-based approach is that
 * the data structure must be constrained to have a traversal only in one direction
 * as explained for the doubly linked list.
 *
 * 
 * 2nd (lock-free) approach:
 * remember that the 1st and this 2nd approaches
 * do not refer to a persistent data structure.
 * the DAG in this 2nd approach is mutating by the user actions.
 * 
 * as described for the 1st option,
 * there are mainly two data shared by
 * the main and the background threads
 * in case of a mutating DAG:
 *     node state data and node relation data
 * node state data could be stored in a node class
 * as an atomic type for the thread safety.
 * however, the same does not apply
 * to the ancestor and descendant nodes
 * due to two reasons:
 *     1. the ancestor and descendant relations are coupled
 *        as explained in the 1st approach.
 *        hence, like the lock-based approach,
 *        the ancestor and descendant relations
 *        must be loaded and stored together
 *        by atomic operations
 *        which is not possible due to the reasons
 *        explained in the 1st approach.
 *     2. ancestor and descendant relations are runtime dependent
 *        and stored in a std::vector<std::size_t> each.
 *        the elements of a vector (i.e. std::size_t) can be treated atomically
 *        while the vector itself is not an atomic type.
 *
 * on the other hand, the lock-free approach
 * could be mixed with the 3rd option below
 * which is a persistent DAG with an inner node class (e.g. PersistentDAG_2.h).
 * the nodes store the state data via an atomic member.
 * problem with this mixed approach is that
 * the atomic types are neither copyable nor movable.
 * the state of each node must be loaded and stored by atomic operations.
 * this reality cancels out all the efficiency
 * of the copy constructors (expecially the bitwise copy)
 * of the contiguous containers (std::vector or std::array)
 * storing the node state data.
 * hence, the most important requirement of a persistent data structure
 * (i.e. the efficient copy constructor)
 * is not satisfied.
 * 
 * 
 * 3rd (persistent DAG) approach:
 * the 3rd approach for the concurrency
 * comes from the Functional Programming (FP).
 * i will not go through the details about the persistent data structures.
 * in summary, this option provides a clean solution to the problem
 * if the copy constructor of the DAG can be implemented efficiently.
 * this 1st version provides an effective copy constructor
 * following the data structures and algorithms
 * arised from the procedural languages.
 * the 2nd version (PersistentDAG_2.h) improves the approach further.
 * 
 * any operation executed on the persistent DAG
 * will follow the following steps:
 *     1. copy the DAG
 *     2. apply the operation on the new DAG
 * these two steps are necessary and sufficient
 * if the DAG is in a valid state and
 * if the operation is not a modification.
 * the relations in a DAG can be so complex that
 * a change in the state of a node can affect a distant node.
 * each modification on a DAG requires
 * some portion of DAG to be inspected
 * when the state of any node has changed.
 * hence, the required steps become:
 *     1. copy the DAG
 *     2. apply the operation on the new DAG
 *     3. (if...) inspect the sub-DAG for the states
 *        by a traversal starting from the modified node
 *
 * the 2nd process can be assumed constant time.
 * the time complexities for the other two (copy and traversal)
 * are the same which is O(kN).
 * the 1st one can be improved by improving the cache usage
 * and the bitwise copy.
 * however, the 3rd one cannot use the cache effectively
 * as the DAG is not a linear data structure.
 * the ancestors of a node can be very far from the node
 * which would not be loaded in the cache together with the node.
 * hence, the 1st process is more and more faster than the 3rd one
 * which ensures that the multithreaded solution with persistent data structure
 * is more and more efficient than the single threaded solution
 * as it isolates the 3rd process to a background thread.
 *
 * now, lets inspect the copy constructor in detail.
 * firstly, a DAG is a link based data structure like a tree.
 * link based data structures are usualy implemented using a node class
 * and defining the node relations via pointers.
 * hence, a traditional (single threaded) DAG would have:
 *     1. inner node class with
 *         * ancestor nodes defined by a vector of raw pointers
 *         * descendant nodes defined by a vector of shared pointers
 *     2. tail node
 *     3. head node
 * the copy constructor of the DAG above would copy the head node
 * which will triger the copy constructors of all nodes recursively.
 * we have the following problems with this copy constructor:
 *     1. too many pointer indirection
 *     2. pointer indirection is a sign of bad cache usage
 *     3. the pointers must be updated after copying the nodes
 *     4. many-to-one relations force us to keep track of the nodes copied (std::unordered_set)
 *     5. recursive process would probably cause stack overflow for a large DAG.
 * a solution to the 1st issue is storing the nodes in the DAG in a
 * contiguous container (std::vector<node>)
 * and replacing the shared pointers of descendant container
 * with raw pointers or weak pointers.
 * now all issues accept for the 3rd one is solved
 * as the ancestor and descendant nodes still point
 * to the nodes of the old DAG.
 * the solution is again provided by the contiguous memory allocation of
 * std::vector.
 * we can determine the location of each node relative to the beginning of
 * the original vector's data
 * and relocate the new pointer using the location relative to the copied
 * vector's data.
 * consider we store the nodes in the DAG like: std::vector<node> _nodes;
 *     1. copy _nodes vector
 *        _nodes = rhs._nodes
 *     2. loop through _nodes
 *         for (auto& new_node : _nodes)
 *     3. loop through the ancestor nodes of the node
 *         for (node* ancestor_node : new_node._ancestor_nodes);
 *     4. relocate the ancestor pointer
 *         auto new_ancestor_node = _nodes.data() + (ancestor_node - rhs._nodes.data())
 * steps 3 and 4 must be repeated for the descendant nodes.
 * the runtime complexity for this copy constructor is 2 * O(k1k2N)
 * where k1 and k2 are the average number of
 * ancestor and descendant nodes for a
 * node respectively.
 * constant 2 comes from the fact that
 * both the vector copy constructor and
 * the pointer correction has the same complexity.
 * however, the time complexity is misleading
 * as the vector's copy constructor
 * would benefit the cache optimization.
 *
 * the key point in the above solution is
 * using the relative adresses pointed
 * by the pointers
 * which is provided by std::vector contiguous allocation.
 * hence, a further optimization would be achieved by
 * using the relative addresses (i.e. indices) directly
 * in the definition of the node relations instead of the pointers.
 * this will cancel out the need for the 2nd process
 * and the copy constructor of the DAG would process
 * only the vector's copy constructor which is very efficient.
 * additionally, now, the vector's copy constructor
 * performs bitwise copy with std::memcpy
 * as std::size_t is trivially copyable
 * which increases the performance further.
 * my tests show that the vector's copy constructor is around 5 times faster
 * with std::size_t comparing to std::vector<node>
 * with a simple non-trivially copyable node definition.
 *
 * obviouslly, the problems of working with indices
 * are the erase and insert functions.
 * the insert function is not needed
 * as the DAG is a link-based data structure
 * and so it does not need to preserve the order of the nodes
 * while storing them in std::vector.
 * hence, insert member function can be treated as a push-back.
 * with the index-based approach,
 * the time complexity of the erase member function is O(k1k2N)
 * as all the indices after the erased index must be decremented.
 * the solution is replacing the erase member function by
 * erase_by_swap_and_pop
 * which swaps the last element with the element to be erased and pop the
 * container.
 * now, the element is erased by changing
 * the index for only the last element
 * which is O(1).
 * the node relations including the last index can easily be updated.
 * however, there is one more work to do that
 * the contained type T belonging to the last element must be informed.
 * this is the 2nd INTERFACE OF THE DAG WITH THE CONTAINED TYPE T:
 * void set_DAG_node(_a_node_var_raw node)
 *
 * defining the noode relations via indices
 * has an enhancement on the iteration for large DAGs.
 * see ITERATORS section for the details.
 * 
 * 
 * on the other hand, the persistent DAG approach comes with a problem:
 *     the node state data is shared between
 *     the main and the background threads because
 *     the main thread copies the node state data
 *     together with the other members of the DAG
 *     while the background thread modifies it.
 * this problem is solved by securing
 * the node state data by a mutex.
 * the main thread works as follows:
 *     1. copy all members of the current DAG accept for the node state data
 *     2. acquire the lock on the mutex
 *     3. copy the node state data (std::vector<unsigned char>)
 *     4. release the lock
 *     5. perform the requested operation on the copy of the DAG
 *     6. return the modified copy of the DAG
 * the background thread works as follows:
 *     1. acquire the lock on the mutex
 *     2. create a copy of the node state data
 *     3. release the lock
 *     4. perform a traversal through the DAG
 *        while mutating the copy of the node state data
 *     5. acquire the lock on the mutex again
 *     6. copy the modified copy of the state data onto the original state data
 *     7. release the lock
 *     8. update the state of the DAG according to the final node state data.
 * hence, the critical section for the mutex is
 * the node state data copy process
 * which is guaranteed to be very fast
 * as its a bitwise copy process.
 * hence, in this approach,
 * the runtime of the main thread is very short
 * which results with a very fast application
 * from the user's point of view.
 * 
 * besides, one of the advantages of the persistency is that
 * it stores the history of the data structure
 * so that there is no need to implement the command design pattern.
 *
 * in summary, the DAG is implemented using the 3rd approach
 * by injecting the lock-based approach partially
 * into a persistent definition.
 *
 * 
 * additionally, the state of the DAG is an atomic
 * which secures the DAG state without a higher level locking mechanism.
 *
 * 
 * 
 * 
 * 
 *
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
