#pragma once

#include <cdk/ast/lvalue_node.h>
#include <cdk/ast/expression_node.h>

namespace udf {

  /**
   * Class for describing pointer index nodes.
   */
  class pointer_index_node: public cdk::lvalue_node {
    cdk::expression_node *_base;
    cdk::expression_node *_index;

  public:
    pointer_index_node(int lineno, cdk::expression_node *base, cdk::expression_node *index) :
        cdk::lvalue_node(lineno), _base(base), _index(index) {
    }

  public:
    cdk::expression_node* base() {
      return _base;
    }
    cdk::expression_node* index() {
      return _index;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_pointer_index_node(this, level);
    }

  };

} // udf
