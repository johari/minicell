require "json"
require "pp"
require "csv"

require "sinatra"
require "sinatra/json"
require 'sinatra/reloader' if development?

require "jaccard"

class PinboardCorpus

  def initialize path
    file = File.read (File.expand_path path)
    @a = JSON.parse file
    @a.each { |u| u["tags"] = u["tags"].split(" ") }
  end

  def all_entries tags
    desired_keys = ["href", "tags", "description", "time"]

    b = @a.select { |obj| obj["tags"].to_set.superset? tags.to_set }.map { |el|
      el.keep_if { |k, v|
        desired_keys.include? k
      }
    }
  end

  def all_tags
    (self.all_entries []).map { |x| x["tags"] }.flatten.to_set.to_a.sort
  end

  def tags_to_url_hash
  
    tag_to_url = Hash.new do
      Set.new
    end

    self.all_entries([]).each { |entry|
      entry["tags"].each { |tag|
        tag_to_url[tag] = tag_to_url[tag].add(entry["href"])
      }
    }

    tag_to_url.each_pair { |k, v|
      tag_to_url[k] = v.to_a
    }

    tag_to_url
  end

end

class MyApp < Sinatra::Base
  def initialize
    super()
    @db = PinboardCorpus.new "~/Downloads/pinboard_export(1)"
    @tag_to_url = @db.tags_to_url_hash
  end

  before do
    response['Access-Control-Allow-Origin'] = 'http://localhost:8000'
  end
  
  get "/api/*" do

    tags = request.fullpath.split("api/")[1].split("/")

    b = @db.all_entries tags
    json b
  end

  get "/correlate/tag/:tag" do
    res = @db.all_tags.map { |tag|
      [Jaccard.coefficient(@tag_to_url[params[:tag]], @tag_to_url[tag]), tag]
    }

    json res.keep_if { |x| x[0] > 0 and x[1] != params[:tag] }.sort.reverse.map { |x| { "other_tag" => x[1], "score" => x[0] }}
  end

  get "/correlate/all" do
    res = Set.new
    @db.all_tags.each { |tag1|
      @db.all_tags.each { |tag2|
        res.add([Jaccard.coefficient(@tag_to_url[tag1], @tag_to_url[tag2]), [tag1, tag2].sort])
      }
    }
    json res.to_a.sort.reverse
  end
end

MyApp.run!

# puts CSV.generate {|csv|
#   b.map { |obj|
#     csv << (desired_keys.map { |k| obj[k] })
#   }
# }

# File.open("pinboard-jobs.json", "w").write(b.to_json)
