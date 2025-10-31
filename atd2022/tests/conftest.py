from hypothesis import settings

# Set default deadline to 10_000 milliseconds (10 seconds).
settings.register_profile("ci", deadline=10_000)
settings.load_profile("ci")
