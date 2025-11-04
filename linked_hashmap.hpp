/**
 * implement a container like std::linked_hashmap
 */
#ifndef SJTU_LINKEDHASHMAP_HPP
#define SJTU_LINKEDHASHMAP_HPP

// only for std::equal_to<T> and std::hash<T>
#include <functional>
#include <cstddef>
#include "utility.hpp"
#include "exceptions.hpp"

namespace sjtu {
    /**
     * In linked_hashmap, iteration ordering is differ from map,
     * which is the order in which keys were inserted into the map.
     * You should maintain a doubly-linked list running through all
     * of its entries to keep the correct iteration order.
     *
     * Note that insertion order is not affected if a key is re-inserted
     * into the map.
     */

template<
	class Key,
	class T,
	class Hash = std::hash<Key>,
	class Equal = std::equal_to<Key>
> class linked_hashmap {
public:
	/**
	 * the internal type of data.
	 * it should have a default constructor, a copy constructor.
	 * You can use sjtu::linked_hashmap as value_type by typedef.
	 */
	typedef pair<const Key, T> value_type;

private:
	struct Node {
		value_type *data;
		Node *prev;
		Node *next;
		Node *hash_next;  // For chaining in hash table

		Node() : data(nullptr), prev(nullptr), next(nullptr), hash_next(nullptr) {}
		Node(const value_type &val) : data(new value_type(val)), prev(nullptr), next(nullptr), hash_next(nullptr) {}
		~Node() {
			if (data) delete data;
		}
	};

	Node *head;  // Dummy head of doubly-linked list
	Node *tail;  // Dummy tail of doubly-linked list
	Node **buckets;  // Hash table buckets
	size_t bucket_count;
	size_t element_count;
	Hash hashFunc;
	Equal equalFunc;

	static const size_t INITIAL_BUCKET_COUNT = 16;
	static constexpr double MAX_LOAD_FACTOR = 0.75;

	size_t get_bucket_index(const Key &key) const {
		return hashFunc(key) % bucket_count;
	}

	void init_buckets(size_t count) {
		bucket_count = count;
		buckets = new Node*[bucket_count];
		for (size_t i = 0; i < bucket_count; ++i) {
			buckets[i] = nullptr;
		}
	}

	void clear_buckets() {
		if (buckets) {
			delete[] buckets;
			buckets = nullptr;
		}
	}

	void rehash(size_t new_bucket_count) {
		Node **old_buckets = buckets;
		size_t old_bucket_count = bucket_count;

		init_buckets(new_bucket_count);

		// Re-insert all nodes into new hash table
		Node *curr = head->next;
		while (curr != tail) {
			size_t idx = get_bucket_index(curr->data->first);
			curr->hash_next = buckets[idx];
			buckets[idx] = curr;
			curr = curr->next;
		}

		// Delete old buckets array
		if (old_buckets) {
			delete[] old_buckets;
		}
	}

	void check_and_rehash() {
		if (element_count > bucket_count * MAX_LOAD_FACTOR) {
			rehash(bucket_count * 2);
		}
	}

	Node* find_node(const Key &key) const {
		if (element_count == 0) return nullptr;
		size_t idx = get_bucket_index(key);
		Node *curr = buckets[idx];
		while (curr) {
			if (equalFunc(curr->data->first, key)) {
				return curr;
			}
			curr = curr->hash_next;
		}
		return nullptr;
	}

	void insert_to_list_end(Node *node) {
		node->prev = tail->prev;
		node->next = tail;
		tail->prev->next = node;
		tail->prev = node;
	}

	void remove_from_list(Node *node) {
		node->prev->next = node->next;
		node->next->prev = node->prev;
	}

	void insert_to_hash_table(Node *node) {
		size_t idx = get_bucket_index(node->data->first);
		node->hash_next = buckets[idx];
		buckets[idx] = node;
	}

	void remove_from_hash_table(Node *node) {
		size_t idx = get_bucket_index(node->data->first);
		Node *curr = buckets[idx];
		Node *prev = nullptr;
		while (curr) {
			if (curr == node) {
				if (prev) {
					prev->hash_next = curr->hash_next;
				} else {
					buckets[idx] = curr->hash_next;
				}
				break;
			}
			prev = curr;
			curr = curr->hash_next;
		}
	}

public:
	/**
	 * see BidirectionalIterator at CppReference for help.
	 *
	 * if there is anything wrong throw invalid_iterator.
	 *     like it = linked_hashmap.begin(); --it;
	 *       or it = linked_hashmap.end(); ++end();
	 */
	class const_iterator;
	class iterator {
		friend class linked_hashmap;
		friend class const_iterator;
	private:
		Node *node_ptr;
		const linked_hashmap *map_ptr;

	public:
		// The following code is written for the C++ type_traits library.
		// Type traits is a C++ feature for describing certain properties of a type.
		// For instance, for an iterator, iterator::value_type is the type that the
		// iterator points to.
		// STL algorithms and containers may use these type_traits (e.g. the following
		// typedef) to work properly.
		// See these websites for more information:
		// https://en.cppreference.com/w/cpp/header/type_traits
		// About value_type: https://blog.csdn.net/u014299153/article/details/72419713
		// About iterator_category: https://en.cppreference.com/w/cpp/iterator
		using difference_type = std::ptrdiff_t;
		using value_type = typename linked_hashmap::value_type;
		using pointer = value_type*;
		using reference = value_type&;
		using iterator_category = std::output_iterator_tag;

		iterator() : node_ptr(nullptr), map_ptr(nullptr) {}

		iterator(Node *node, const linked_hashmap *map) : node_ptr(node), map_ptr(map) {}

		iterator(const iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}

		/**
		 * iter++
		 */
		iterator operator++(int) {
			if (node_ptr == map_ptr->tail) {
				throw invalid_iterator();
			}
			iterator temp = *this;
			node_ptr = node_ptr->next;
			return temp;
		}

		/**
		 * ++iter
		 */
		iterator & operator++() {
			if (node_ptr == map_ptr->tail) {
				throw invalid_iterator();
			}
			node_ptr = node_ptr->next;
			return *this;
		}

		/**
		 * iter--
		 */
		iterator operator--(int) {
			if (node_ptr == map_ptr->head->next) {
				throw invalid_iterator();
			}
			iterator temp = *this;
			node_ptr = node_ptr->prev;
			return temp;
		}

		/**
		 * --iter
		 */
		iterator & operator--() {
			if (node_ptr == map_ptr->head->next) {
				throw invalid_iterator();
			}
			node_ptr = node_ptr->prev;
			return *this;
		}

		/**
		 * a operator to check whether two iterators are same (pointing to the same memory).
		 */
		value_type & operator*() const {
			return *(node_ptr->data);
		}

		bool operator==(const iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}

		bool operator==(const const_iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}

		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const {
			return !(*this == rhs);
		}

		bool operator!=(const const_iterator &rhs) const {
			return !(*this == rhs);
		}

		/**
		 * for the support of it->first.
		 * See <http://kelvinh.github.io/blog/2013/11/20/overloading-of-member-access-operator-dash-greater-than-symbol-in-cpp/> for help.
		 */
		value_type* operator->() const noexcept {
			return node_ptr->data;
		}
	};

	class const_iterator {
		friend class linked_hashmap;
		friend class iterator;
	private:
		Node *node_ptr;
		const linked_hashmap *map_ptr;

	public:
		using difference_type = std::ptrdiff_t;
		using value_type = typename linked_hashmap::value_type;
		using pointer = const value_type*;
		using reference = const value_type&;
		using iterator_category = std::output_iterator_tag;

		const_iterator() : node_ptr(nullptr), map_ptr(nullptr) {}

		const_iterator(Node *node, const linked_hashmap *map) : node_ptr(node), map_ptr(map) {}

		const_iterator(const const_iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}

		const_iterator(const iterator &other) : node_ptr(other.node_ptr), map_ptr(other.map_ptr) {}

		const_iterator operator++(int) {
			if (node_ptr == map_ptr->tail) {
				throw invalid_iterator();
			}
			const_iterator temp = *this;
			node_ptr = node_ptr->next;
			return temp;
		}

		const_iterator & operator++() {
			if (node_ptr == map_ptr->tail) {
				throw invalid_iterator();
			}
			node_ptr = node_ptr->next;
			return *this;
		}

		const_iterator operator--(int) {
			if (node_ptr == map_ptr->head->next) {
				throw invalid_iterator();
			}
			const_iterator temp = *this;
			node_ptr = node_ptr->prev;
			return temp;
		}

		const_iterator & operator--() {
			if (node_ptr == map_ptr->head->next) {
				throw invalid_iterator();
			}
			node_ptr = node_ptr->prev;
			return *this;
		}

		const value_type & operator*() const {
			return *(node_ptr->data);
		}

		bool operator==(const iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}

		bool operator==(const const_iterator &rhs) const {
			return node_ptr == rhs.node_ptr && map_ptr == rhs.map_ptr;
		}

		bool operator!=(const iterator &rhs) const {
			return !(*this == rhs);
		}

		bool operator!=(const const_iterator &rhs) const {
			return !(*this == rhs);
		}

		const value_type* operator->() const noexcept {
			return node_ptr->data;
		}
	};

	/**
	 * two constructors
	 */
	linked_hashmap() : element_count(0) {
		head = new Node();
		tail = new Node();
		head->next = tail;
		tail->prev = head;
		init_buckets(INITIAL_BUCKET_COUNT);
	}

	linked_hashmap(const linked_hashmap &other) : element_count(0) {
		head = new Node();
		tail = new Node();
		head->next = tail;
		tail->prev = head;
		init_buckets(other.bucket_count);

		// Copy all elements
		Node *curr = other.head->next;
		while (curr != other.tail) {
			insert(*(curr->data));
			curr = curr->next;
		}
	}

	/**
	 * assignment operator
	 */
	linked_hashmap & operator=(const linked_hashmap &other) {
		if (this == &other) return *this;

		clear();
		clear_buckets();
		init_buckets(other.bucket_count);

		// Copy all elements
		Node *curr = other.head->next;
		while (curr != other.tail) {
			insert(*(curr->data));
			curr = curr->next;
		}

		return *this;
	}

	/**
	 * Destructors
	 */
	~linked_hashmap() {
		clear();
		delete head;
		delete tail;
		clear_buckets();
	}

	/**
	 * access specified element with bounds checking
	 * Returns a reference to the mapped value of the element with key equivalent to key.
	 * If no such element exists, an exception of type `index_out_of_bound'
	 */
	T & at(const Key &key) {
		Node *node = find_node(key);
		if (!node) {
			throw index_out_of_bound();
		}
		return node->data->second;
	}

	const T & at(const Key &key) const {
		Node *node = find_node(key);
		if (!node) {
			throw index_out_of_bound();
		}
		return node->data->second;
	}

	/**
	 * access specified element
	 * Returns a reference to the value that is mapped to a key equivalent to key,
	 *   performing an insertion if such key does not already exist.
	 */
	T & operator[](const Key &key) {
		Node *node = find_node(key);
		if (node) {
			return node->data->second;
		}

		// Insert new element with default value
		value_type new_val(key, T());
		auto result = insert(new_val);
		return result.first->second;
	}

	/**
	 * behave like at() throw index_out_of_bound if such key does not exist.
	 */
	const T & operator[](const Key &key) const {
		return at(key);
	}

	/**
	 * return a iterator to the beginning
	 */
	iterator begin() {
		return iterator(head->next, this);
	}

	const_iterator cbegin() const {
		return const_iterator(head->next, this);
	}

	/**
	 * return a iterator to the end
	 * in fact, it returns past-the-end.
	 */
	iterator end() {
		return iterator(tail, this);
	}

	const_iterator cend() const {
		return const_iterator(tail, this);
	}

	/**
	 * checks whether the container is empty
	 * return true if empty, otherwise false.
	 */
	bool empty() const {
		return element_count == 0;
	}

	/**
	 * returns the number of elements.
	 */
	size_t size() const {
		return element_count;
	}

	/**
	 * clears the contents
	 */
	void clear() {
		Node *curr = head->next;
		while (curr != tail) {
			Node *next = curr->next;
			delete curr;
			curr = next;
		}
		head->next = tail;
		tail->prev = head;

		// Clear hash table
		for (size_t i = 0; i < bucket_count; ++i) {
			buckets[i] = nullptr;
		}

		element_count = 0;
	}

	/**
	 * insert an element.
	 * return a pair, the first of the pair is
	 *   the iterator to the new element (or the element that prevented the insertion),
	 *   the second one is true if insert successfully, or false.
	 */
	pair<iterator, bool> insert(const value_type &value) {
		Node *existing = find_node(value.first);
		if (existing) {
			return pair<iterator, bool>(iterator(existing, this), false);
		}

		// Create new node
		Node *new_node = new Node(value);

		// Insert into doubly-linked list at the end
		insert_to_list_end(new_node);

		// Insert into hash table
		insert_to_hash_table(new_node);

		element_count++;
		check_and_rehash();

		return pair<iterator, bool>(iterator(new_node, this), true);
	}

	/**
	 * erase the element at pos.
	 *
	 * throw if pos pointed to a bad element (pos == this->end() || pos points an element out of this)
	 */
	void erase(iterator pos) {
		if (pos.map_ptr != this || pos.node_ptr == tail || pos.node_ptr == head) {
			throw invalid_iterator();
		}

		Node *node = pos.node_ptr;

		// Remove from doubly-linked list
		remove_from_list(node);

		// Remove from hash table
		remove_from_hash_table(node);

		element_count--;
		delete node;
	}

	/**
	 * Returns the number of elements with key
	 *   that compares equivalent to the specified argument,
	 *   which is either 1 or 0
	 *     since this container does not allow duplicates.
	 */
	size_t count(const Key &key) const {
		return find_node(key) ? 1 : 0;
	}

	/**
	 * Finds an element with key equivalent to key.
	 * key value of the element to search for.
	 * Iterator to an element with key equivalent to key.
	 *   If no such element is found, past-the-end (see end()) iterator is returned.
	 */
	iterator find(const Key &key) {
		Node *node = find_node(key);
		if (node) {
			return iterator(node, this);
		}
		return end();
	}

	const_iterator find(const Key &key) const {
		Node *node = find_node(key);
		if (node) {
			return const_iterator(node, this);
		}
		return cend();
	}
};

}

#endif
