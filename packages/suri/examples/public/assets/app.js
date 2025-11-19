// Static Files Example JavaScript

console.log('✅ Static Files Middleware loaded!');
console.log('📦 This JavaScript file was served with Content-Type: text/javascript');

// Add some interactivity
document.addEventListener('DOMContentLoaded', () => {
  console.log('🎉 Page loaded successfully!');
  
  // Log cache headers if available
  const logCacheHeaders = () => {
    fetch(window.location.href, { method: 'HEAD' })
      .then(response => {
        console.log('📋 Cache Headers:');
        console.log('  ETag:', response.headers.get('etag'));
        console.log('  Last-Modified:', response.headers.get('last-modified'));
        console.log('  Cache-Control:', response.headers.get('cache-control'));
      })
      .catch(err => console.error('Failed to fetch headers:', err));
  };
  
  // Log headers after a short delay
  setTimeout(logCacheHeaders, 500);
  
  // Add click handlers for security test links
  const securityLinks = document.querySelectorAll('a[href*="passwd"], a[href*=".env"]');
  securityLinks.forEach(link => {
    link.addEventListener('click', (e) => {
      e.preventDefault();
      const url = link.getAttribute('href');
      
      fetch(url)
        .then(response => {
          if (response.status === 403) {
            alert(`✅ Security working! ${url} was blocked with 403 Forbidden`);
          } else {
            alert(`⚠️ Unexpected response: ${response.status}`);
          }
        })
        .catch(err => {
          alert(`❌ Error: ${err.message}`);
        });
    });
  });
});

// Demonstrate that this is real JavaScript with a simple function
const greet = (name = 'Static Files Middleware') => {
  return `Hello from ${name}! 🚀`;
};

console.log(greet());
