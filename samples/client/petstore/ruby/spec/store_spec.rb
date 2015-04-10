require 'spec_helper'

describe "Store" do
  before do
    configure_swagger
  end

  it "should fetch an order" do
    item = StoreApi.get_order_by_id(10002)
    item.id.should == 10002
  end
end
