#pragma once

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace udf {

  /**
   * Class for describing iterate nodes.
   */
  class iterate_node : public cdk::basic_node {
    cdk::sequence_node *_vector;
    cdk::expression_node *_count;
    std::string _identifier;
    cdk::expression_node *_condition;

    cdk::basic_node *_block;

  public:
    iterate_node(int lineno, cdk::expression_node *vector, cdk::basic_node *count, 
        const std::string &identifier, cdk::expression_node *_condition) :
        cdk::basic_node(lineno), _vector(vector), _count(count),_identifier(identifier),
        _condition(condition) {
    }

     const std::string& identifier() {
      return _identifier;
    }

    cdk::expression_node* vector(size_t ix) {
      return (cdk::expression_node*)_vector->node(ix);
    }

    cdk::sequence_node *fullvector() {
      return _vector;
    }

    cdk::expression_node *count() { return _count; }

    cdk::expression_node *condition() { return _condition; }

    void accept(basic_ast_visitor *sp, int level) { sp->do_iterate_node(this, level); }

  };

} // udf
