task :default => :build_all

task :build_all => [:build_backend, :build_frontend]

task :build_backend do
    sh("stack build")
end

task :build_frontend => :elm

task :elm => [:elm_app_js, :elm_api_code_generator]

task :elm_app_js do
    mkdir_p "frontend/src/"
    sh("elm-make frontend/src/Main.elm --output frontend/dist/app.js")
end

task :elm_api_code_generator do
    mkdir_p "code-generator/"
    sh("stack exec code-generator")
end

task :serve do
    sh("stack exec backend")
end