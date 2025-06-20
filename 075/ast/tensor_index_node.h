#pragma once

#include <cdk/ast/lvalue_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  /**
   * Class for describing tensor index nodes.
   */
   class tensor_index_node: public cdk::lvalue_node {
    cdk::expression_node *_base;
    cdk::sequence_node * _positions;

  public:
    tensor_index_node(int lineno, cdk::expression_node *base, cdk::sequence_node *positions) :
        cdk::lvalue_node(lineno), _base(base), _positions(positions) {
    }

  public:
    cdk::expression_node* position(size_t ix) {
      return (cdk::expression_node*)_positions->node(ix);
    }
    
    cdk::expression_node *base() {
      return _base;
    }
    cdk::sequence_node *positions() {
      return _positions;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_tensor_index_node(this, level);
    }

  };

} // udf